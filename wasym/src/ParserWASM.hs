module ParserWASM (parseWASM, moduleP) where

import Text.Megaparsec
import Text.Megaparsec.Byte

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Bifunctor (first)
import Control.Monad (join)
import Data.Word (Word32, Word64)
import qualified Data.Vector as V
import Data.Either (fromRight)

import qualified Data.ByteString as B
import Data.Text.Encoding (decodeUtf8)
import Data.Serialize.IEEE754
import Data.Serialize.Get (runGet)
import qualified Data.Text as Text (unpack)

import AST
import ParserInstr

type Parser a = Parsec ParserWASMError B.ByteString a

data ParserWASMError = ULEB128 deriving (Show, Eq, Ord)

instance ShowErrorComponent ParserWASMError where
  showErrorComponent = show
--------------------------------------------------------------------------------
-- * Primitives
--------------------------------------------------------------------------------

unsignedLEB128 :: Integral a => Integer -> Parser a
unsignedLEB128 size =
  fromIntegral <$> anySingle >>= \ n ->
  if n < 128 && ((fromIntegral n) :: Integer) < 2 ^ size
    then pure n
  else if size > 7
    then (\ m -> 128 * m + n - 128) <$> unsignedLEB128 (size - 7)
  else customFailure ULEB128

u32 :: Parser Word32
u32 = unsignedLEB128 32 <?> "u32"

u64 :: Parser Word64
u64 = unsignedLEB128 64 <?> "u64"

f32 :: Parser Float
f32 =
  takeP Nothing 4 <&>
  runGet getFloat32le <&>
  fromRight (error "failed to read float")
  <?> "f32"

f64 :: Parser Double
f64 =
  takeP Nothing 8 <&>
  runGet getFloat64le <&>
  fromRight (error "failed to read float")
  <?> "f64"

idx :: Parser Idx
idx = u32 <?> "idx"

vec :: Parser a -> Parser (Vec a)
vec p =
  u32 >>= \ elemCount ->
  V.replicate (fromIntegral elemCount) p &
  sequenceA

vecTill :: Parser a -> Parser () -> Parser (Vec a)
vecTill p pEnd = V.fromList <$> manyTill p pEnd

byte :: Byte -> Parser ()
byte b =
  () <$ char b

name :: Parser String
name =
  u32 >>=
  takeP Nothing . fromIntegral <&>
  Text.unpack . decodeUtf8
  <?> "name"

--------------------------------------------------------------------------------
-- * Lesser types
--------------------------------------------------------------------------------

valType :: Parser ValType
valType =
  (NumInt, W32) <$ byte 0x7F <|>
  (NumInt, W64) <$ byte 0x7E <|>
  (NumFlt, W32) <$ byte 0x7D <|>
  (NumFlt, W64) <$ byte 0x7C
  <?> "valtype"

resType :: Parser ResType
resType =
  Nothing <$ byte 0x40 <|>
  Just <$> valType
  <?> "restype"

funcTypeP :: Parser FuncType
funcTypeP =
  byte 0x60 *> (
    FuncType
      <$> vec valType
      <*> (vec valType <&> (V.!? 1)) -- Just a single result in spec 1.0
  )
  <?> "functype"

limits :: Parser Limits
limits =
  byte 0x00 *> (Limits <$> u32 <*> pure Nothing) <|>
  byte 0x01 *> (Limits <$> u32 <*> (Just <$> u32))
  <?> "limits"

memType :: Parser MemType
memType = limits <?> "memtype"

tableType :: Parser TableType
tableType =
  flip TableType <$> elemType <*> limits
  <?> "tabletype"

elemType :: Parser ElemType
elemType =
  Undefined <$ byte 0x70 -- FuncRef type seems to contain a lot of redundancy
  <?> "elemtype"

globalTypeP :: Parser GlobalType
globalTypeP =
  flip GlobalType <$> valType <*> (False <$ byte 0x00 <|> True <$ byte 0x01)
  <?> "globalType"

--------------------------------------------------------------------------------
-- * Instructions
--------------------------------------------------------------------------------

instr :: Parser Instr
instr =
  controlInstr
  <|> paramInstr
  <|> varInstr
  <|> memInstr
  <|> constInstr
  <|> numInstr
  <?> "instr"

controlInstr :: Parser Instr
controlInstr =
  Unreachable <$ byte 0x00 <|>
  Nop <$ byte 0x01 <|>
  Return <$ byte 0x0F <|>
  byte 0x0C *> (Branch       <$> idx)              <|>
  byte 0x0D *> (BranchIf     <$> idx)              <|>
  byte 0x0E *> (BranchTable  <$> vec idx <*> idx)  <|>
  byte 0x10 *> (Call         <$> idx)              <|>
  byte 0x11 *> (CallIndirect <$> idx) <* byte 0x00 <|>
  blockInstr <|>
  loopInstr <|>
  ifElseInstr

end :: Parser ()
end = byte 0x0B <?> "end"

blockInstr :: Parser Instr
blockInstr =
  byte 0x02 *> (
  Block
    <$> resType
    <*> vecTill instr end
  )

loopInstr :: Parser Instr
loopInstr =
  byte 0x03 *> (
  Loop
    <$> resType
    <*> vecTill instr end
  )

ifElseInstr :: Parser Instr
ifElseInstr =
  byte 0x04 *> (
  If
    <$> resType
    <*> vecTill instr (lookAhead $ byte 0x05 <|> end)
    <*> ((byte 0x05 *> vecTill instr end) <|> (end *> pure V.empty))
  )

paramInstr :: Parser Instr
paramInstr =
  Drop <$ byte 0x1A <|>
  Select <$ byte 0x1B

varInstr :: Parser Instr
varInstr =
  byte 0x20 *> (LocalGet <$> idx) <|>
  byte 0x21 *> (LocalSet <$> idx) <|>
  byte 0x22 *> (LocalTee <$> idx) <|>
  byte 0x23 *> (GlobalGet <$> idx) <|>
  byte 0x24 *> (GlobalSet <$> idx)

--------------------------------------------------------------------------------
-- * Memory instructions
--------------------------------------------------------------------------------

memArg :: Parser MemArg
memArg =
  flip MemArg <$> u32 <*> u32
  <?> "memarg"

memInstr :: Parser Instr
memInstr =
  byte 0x28 *> (Load (NumInt, W32) <$> memArg) <|>
  byte 0x29 *> (Load (NumInt, W64) <$> memArg) <|>
  byte 0x2A *> (Load (NumFlt, W32) <$> memArg) <|>
  byte 0x2B *> (Load (NumFlt, W64) <$> memArg) <|>
  byte 0x2C *> (IntLoad8  W32 SN   <$> memArg) <|>
  byte 0x2D *> (IntLoad8  W32 US <$> memArg) <|>
  byte 0x2E *> (IntLoad16 W32 SN   <$> memArg) <|>
  byte 0x2F *> (IntLoad16 W32 US <$> memArg) <|>
  byte 0x30 *> (IntLoad8  W64 SN   <$> memArg) <|>
  byte 0x31 *> (IntLoad8  W64 US <$> memArg) <|>
  byte 0x32 *> (IntLoad16 W64 SN   <$> memArg) <|>
  byte 0x33 *> (IntLoad16 W64 US <$> memArg) <|>
  byte 0x34 *> (IntLoad32 SN <$> memArg) <|>
  byte 0x35 *> (IntLoad32 US <$> memArg) <|>
  byte 0x36 *> (Store (NumInt, W32) <$> memArg) <|>
  byte 0x37 *> (Store (NumInt, W64) <$> memArg) <|>
  byte 0x38 *> (Store (NumFlt, W32) <$> memArg) <|>
  byte 0x39 *> (Store (NumFlt, W64) <$> memArg) <|>
  byte 0x3A *> (IntStore8  W32 <$> memArg) <|>
  byte 0x3B *> (IntStore16 W32 <$> memArg) <|>
  byte 0x3C *> (IntStore8  W64 <$> memArg) <|>
  byte 0x3D *> (IntStore16 W64 <$> memArg) <|>
  byte 0x3E *> (IntStore32 <$> memArg) <|>
  byte 0x3F *> byte 0x3F *> pure MemorySize <|>
  byte 0x40 *> byte 0x00 *> pure MemoryGrow

--------------------------------------------------------------------------------
-- * Numerical instructions
--------------------------------------------------------------------------------

constInstr :: Parser Instr
constInstr =
  byte 0x41 *> (Constant . I32Val <$> u32) <|>
  byte 0x42 *> (Constant . I64Val <$> u64) <|>
  byte 0x43 *> (Constant . F32Val <$> f32) <|>
  byte 0x44 *> (Constant . F64Val <$> f64)

numInstr :: Parser Instr
numInstr =
  anySingle >>= \ b ->
  lookupInstr b & maybe empty pure

--------------------------------------------------------------------------------
-- * Expressions
--------------------------------------------------------------------------------

expr :: Parser Expr
expr = vecTill instr end <?> "expr"

--------------------------------------------------------------------------------
-- * Constructs
--------------------------------------------------------------------------------
importP :: Parser Import
importP =
  Import <$> name <*> name <*> importDescP
  <?> "import"

importDescP :: Parser ImportDesc
importDescP =
  byte 0x00 *> (ImportFunc <$> idx) <|>
  byte 0x01 *> (ImportTable <$> idx) <|>
  byte 0x02 *> (ImportMem <$> idx) <|>
  byte 0x03 *> (ImportGlobal <$> idx)
  <?> "importdesc"

global :: Parser Global
global =
  Global <$> globalTypeP <*> expr
  <?> "global"

export :: Parser Export
export =
  Export <$> name <*> exportDesc
  <?> "export"

exportDesc :: Parser ExportDesc
exportDesc =
  byte 0x00 *> (ExportFunc <$> idx) <|>
  byte 0x01 *> (ExportTable <$> idx) <|>
  byte 0x02 *> (ExportMem <$> idx) <|>
  byte 0x03 *> (ExportGlobal <$> idx)
  <?> "exportdesc"

elemP :: Parser Elem
elemP =
  Elem <$> idx <*> expr <*> vec idx
  <?> "elem"

bodyP :: Parser (Vec ValType, Expr)
bodyP =
  u32 *> ((,) <$> localsP <*> expr)

localsP :: Parser (Vec ValType)
localsP =
  join <$> vec (
      V.replicate <$> (fromIntegral <$> u32) <*> valType
  )

makeFunc :: Vec FuncType -> Idx -> (Vec ValType, Expr) -> Func
makeFunc funcTypes funcIdx (valTypes, funcBody) =
  let funcType = funcTypes V.! (fromIntegral funcIdx) in
  Func funcType valTypes funcBody

dataP :: Parser Data
dataP =
  Data <$> idx <*> expr <*> vec anySingle
  <?> "data"

--------------------------------------------------------------------------------
-- * Modules
--------------------------------------------------------------------------------

customSection :: Parser ()
customSection =
  byte 0x00 *>
  (u32 >>= takeP (Just "custom section") . fromIntegral) *>
  pure ()
  <?> "custom section"

section :: Byte -> a -> Parser a -> Parser a
section nID fallback p =
  (byte nID *> u32 *> p <|> pure fallback) <* skipMany customSection

vecSection :: Byte -> Parser a -> Parser (Vec a)
vecSection nID p = section nID V.empty (vec p)

moduleP :: Parser Module
moduleP =
  do
    _ <- chunk (B.pack [0x00, 0x61, 0x73, 0x6D, 0x01, 0x00, 0x00, 0x00])
    skipMany customSection
    mTypes <- vecSection 1 funcTypeP
    mImports <- vecSection 2 importP
    mFuncIdxs <- vecSection 3 idx
    mTables <- vecSection 4 tableType
    mMemory <- vecSection 5 memType
    mGlobals <- vecSection 6 global
    mExports <- vecSection 7 export
    mStart <- section 8 Nothing (optional idx)
    mElems <- vecSection 9 elemP
    mCode <- vecSection 10 bodyP
    mDatas <- vecSection 11 dataP
    eof

    return $ Module {
      types = mTypes,
      funcs = V.zipWith (makeFunc mTypes) mFuncIdxs mCode,
      tables = mTables,
      mems = mMemory,
      globals = mGlobals,
      elems = mElems,
      dat = mDatas,
      start = mStart,
      imports = mImports,
      exports = mExports
    }

parseWASM :: String -> B.ByteString -> Either String Module
parseWASM filename text =
  parse moduleP filename text & first errorBundlePretty

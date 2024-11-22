module AST where

import Data.Word
import qualified Data.Vector as V

type Byte = Word8
type Idx = Word32
type Vec = V.Vector

-- This module implements an abstract syntax tree in accordance with the
-- structure section of the WASM spec 1.0
-- To be used as static data for the interpreter

-- TODO:
-- * Support symbolic identifiers from the textual format, for user-friendliness
-- * Are external types necessary?

-- * This type, FuncRef, figure out what it is and should be:
-- I'm guessing it's for foreign function interfaces.
data FuncRef = Undefined deriving (Show)

data Module = Module {
  types   :: Vec FuncType,
  funcs   :: Vec Func,
  tables  :: Vec TableType,
  mems    :: Vec MemType,
  globals :: Vec Global,
  elems   :: Vec Elem,
  dat     :: Vec Data,
  start   :: Maybe StartFunc,
  imports :: Vec Import,
  exports :: Vec Export
} deriving (Show)

{----- Component types -----}
data Func = Func {
  fType :: FuncType,
  locals :: Vec ValType,
  body :: Expr
} deriving (Show)

data Global = Global {
  myType :: GlobalType,
  globalInit :: Expr
} deriving (Show)

data Elem = Elem {
  elemRange :: Idx, -- only current valid index is 0
  elemOffset :: Expr, -- must be constant
  elemInit :: Vec Idx
} deriving (Show)

data Data = Data {
  dataRange :: Idx,
  dataOffset :: Expr,
  dataInit :: Vec Byte
} deriving (Show)

-- Function index of start/initialization function
type StartFunc = Idx

data Export = Export Name ExportDesc deriving (Show)

data ExportDesc =
    ExportFunc Idx
  | ExportTable Idx
  | ExportMem Idx
  | ExportGlobal Idx
  deriving (Show)

data Import = Import {
  moduleName :: Name,
  importName :: Name,
  importDesc :: ImportDesc
} deriving (Show)

data ImportDesc =
    ImportFunc Idx
  | ImportTable Idx
  | ImportMem Idx
  | ImportGlobal Idx
  deriving (Show)

{----- Lesser types -----}
type ValType = (NumType, BitWidth)
data NumType = NumInt | NumFlt deriving (Show, Eq)
data BitWidth = W32 | W64 deriving (Show, Eq)
data Sign = SN | US deriving (Show, Eq)

type ResType = Maybe ValType

type Name = String

data FuncType = FuncType {
  funcParams :: Vec ValType,
  funcResult :: Maybe ValType
} deriving (Show)

data Limits = Limits {
  min :: Word32,
  max :: Maybe Word32
} deriving (Show)

type MemType = Limits

data TableType = TableType Limits ElemType deriving (Show)
type ElemType = FuncRef

data GlobalType = GlobalType {
  mutable :: Bool,
  globalType :: ValType
} deriving (Show)

type Expr = Vec Instr

{--
  It is possible to create malformed instructions, but we leave it to the
  parser not to do so.
--}

data Instr =
-- Control Instructions
   Unreachable
 | Nop
 | Block ResType Expr
 | Loop ResType Expr
 | If ResType Expr Expr
 | Branch Idx
 | BranchIf Idx
 | BranchTable (Vec Idx) Idx
 | Return
 | Call Idx
 | CallIndirect Idx
-- Parametric Instructions
 | Drop
 | Select
-- Variable Instructions
 | LocalGet Idx
 | LocalSet Idx
 | LocalTee Idx
 | GlobalGet Idx
 | GlobalSet Idx
-- Memory Instructions
 | MemorySize
 | MemoryGrow
 | Load ValType MemArg
 | Store ValType MemArg
 | IntLoad8 BitWidth Sign MemArg
 | IntLoad16 BitWidth Sign MemArg
 | IntLoad32 Sign MemArg
 | IntStore8 BitWidth MemArg
 | IntStore16 BitWidth MemArg
 | IntStore32 MemArg
-- Numerical Instructions
 | Constant Value
 | IUnary BitWidth IUnOp
 | IBinary BitWidth IBinOp
 | ICompare BitWidth IRelOp
 | FUnary BitWidth FUnOp
 | FBinary BitWidth FBinOp
 | FCompare BitWidth FRelOp
-- More Numerical Instructions
 | WrapI64
 | ExtendI32 Sign
 | IntTruncFlt BitWidth BitWidth Sign
 | FltDemote
 | FltPromote
 | Flt2Int BitWidth BitWidth Sign
 | Reinterpret ValType ValType
 deriving (Show, Eq)

data Value =
    I32Val Word32
  | I64Val Word64
  | F32Val Float
  | F64Val Double
  deriving (Show, Eq)

data MemArg = MemArg {
  memOffset :: Word32,
  memAlign :: Word32
} deriving (Show, Eq)

data IUnOp =
  IClz | ICtz | IPopCnt | IEQZ
  deriving (Show, Eq)

data IBinOp =
  IAdd | ISub | IMul | IDiv Sign | IRem Sign |
  IAnd | IOr | IXOr | IShiftL | IShiftR Sign | IRotL | IRotR
  deriving (Show, Eq)

data IRelOp =
  IEQ | INE | ILT Sign | IGT Sign | ILE Sign | IGE Sign
  deriving (Show, Eq)

data FUnOp =
  FAbs | FNeg | FSqrt | FCeil | FFloor | FTrunc | FNearest
  deriving (Show, Eq)

data FBinOp =
  FAdd | FSub | FMul | FDiv | FMin | FMax | FCopySign
  deriving (Show, Eq)

data FRelOp =
  FEQ | FNE | FLT | FGT | FLE | FGE
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- * Utility
--------------------------------------------------------------------------------
unconsV :: V.Vector a -> Maybe (a, V.Vector a)
unconsV vec =
  if V.null vec
    then Nothing
    else Just (V.head vec, V.tail vec)

getFunc :: Module -> String -> Maybe Idx
getFunc mdl funcName =
  (\ (Export _ (ExportFunc idx)) -> idx) <$>
  V.find (matchExportFunc funcName) (exports mdl)

matchExportFunc :: String -> Export -> Bool
matchExportFunc funcName (Export name (ExportFunc _)) | funcName == name = True
matchExportFunc _ _ = False

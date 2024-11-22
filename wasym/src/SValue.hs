{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module SValue where

import Data.SBV

import AST

data SValue =
    SW32 SWord32
  | SW64 SWord64
  | SF32 SFloat
  | SF64 SDouble

--------------------------------------------------------------------------------
--- * Creation
--------------------------------------------------------------------------------

mkSValue :: ValType -> Symbolic SValue
mkSValue t =
  case t of
    (NumInt, W32) -> SW32 <$> free_
    (NumInt, W64) -> SW64 <$> free_
    (NumFlt, W32) -> SF32 <$> free_
    (NumFlt, W64) -> SF64 <$> free_

-- | Creates a concrete value of 0 corresponding to the given type
mkZero :: ValType -> SValue
mkZero t =
  case t of
    (NumInt, W32) -> SW32 0
    (NumInt, W64) -> SW64 0
    (NumFlt, W32) -> SF32 0
    (NumFlt, W64) -> SF64 0

--------------------------------------------------------------------------------
-- * Operations
--------------------------------------------------------------------------------
type UnOp = SValue -> SValue

type BinOp = SValue -> SValue -> SValue

--evalIUnOp :: IUnOp -> SValue -> SValue
--evalIUnOp op =
--  case op of
--    IClz ->
--    ICtz ->
--    IPopCnt ->
--    IEQZ ->

boolToNum :: SBool -> SValue
boolToNum v = SW32 $ ite v 1 0

getIBinOp :: ( SymVal a,
               Integral a,
               Bits a,
               SDivisible (SBV a)
             ) =>
             IBinOp -> SBV a -> SBV a -> SBV a
getIBinOp op =
  case op of
    IAdd -> (+)
    ISub -> (-)
    IMul -> (*)
    IDiv US -> sDiv
    IRem US -> sRem
    IAnd -> (.&.)
    IOr -> (.|.)
    IXOr -> xor

getIRelOp :: (SymVal a, Ord a) =>  IRelOp -> SBV a -> SBV a -> SValue
getIRelOp op x y =
  case op of
    IEQ ->    boolToNum (x .== y)
    INE ->    boolToNum (x ./= y)
    ILT US -> boolToNum (x .<  y)
    IGT US -> boolToNum (x .>  y)
    ILE US -> boolToNum (x .<= y)
    IGE US -> boolToNum (x .>= y)

evalIRelOp :: IRelOp -> SValue -> SValue -> SValue
evalIRelOp op a b =
  case (a, b) of
    (SW32 x, SW32 y) -> getIRelOp op x y
    (SW64 x, SW64 y) -> getIRelOp op x y

evalIBinOp :: IBinOp -> BinOp
evalIBinOp op a b =
  case (a, b) of
    (SW32 x, SW32 y) -> SW32 $ getIBinOp op x y
    (SW64 x, SW64 y) -> SW64 $ getIBinOp op x y
    _ -> error "type mismatch in binary int operation"

getFBinOp :: (IEEEFloating a) => FBinOp -> SBV a -> SBV a -> SBV a
getFBinOp op =
  case op of
    FAdd -> fpAdd sRoundNearestTiesToEven
    FSub -> fpSub sRoundNearestTiesToEven
    FMul -> fpMul sRoundNearestTiesToEven
    FDiv -> fpDiv sRoundNearestTiesToEven
    FMin -> fpMin
    FMax -> fpMax

evalFBinOp :: FBinOp -> BinOp
evalFBinOp op a b =
  case (a, b) of
    (SF32 x, SF32 y) -> SF32 $ getFBinOp op x y
    (SF64 x, SF64 y) -> SF64 $ getFBinOp op x y
    _ -> error "type mismatch in binary float operation"

--evalFRelOp :: FBinOp -> BinOp
--evalFRelOp op a b =
--  case (a, b) of
--    (SF32 a, SF32 y) -> SW32 $ getFRelOp op x y
--    (SF64 a, SF64 y) -> SW32 $ getFRelOp op x y

vToBytes :: SValue -> [SWord8]
vToBytes val =
  case val of
    SW32 x -> map fromSized . toBytes . toSized $ x
    SW64 x -> map fromSized . toBytes . toSized $ x
    _ -> error "cannot convert between symbolic bytes and floats"

vFromBytes :: ValType -> [SWord8] -> SValue
vFromBytes valType =
  case valType of
    (NumInt, W32) ->
      SW32 . fromSized . (fromBytes :: [SWord 8] -> SWord 32) . map toSized
    (NumInt, W64) ->
      SW64 . fromSized . (fromBytes :: [SWord 8] -> SWord 64) . map toSized
    _ -> error "cannot convert between symbolic bytes and floats"


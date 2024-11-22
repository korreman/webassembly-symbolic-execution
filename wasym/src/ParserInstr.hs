module ParserInstr where

import Data.Word (Word8)
import AST

-- Mapping of bytes to their respective non-parametric instructions

lookupInstr :: Word8 -> Maybe Instr
lookupInstr b =
  case b of
    0x45 -> Just $ IUnary W32 IEQZ
    0x46 -> Just $ ICompare W32 IEQ
    0x47 -> Just $ ICompare W32 INE
    0x48 -> Just $ ICompare W32 (ILT SN)
    0x49 -> Just $ ICompare W32 (ILT US)
    0x4A -> Just $ ICompare W32 (IGT SN)
    0x4B -> Just $ ICompare W32 (IGT US)
    0x4C -> Just $ ICompare W32 (ILE SN)
    0x4D -> Just $ ICompare W32 (ILE US)
    0x4E -> Just $ ICompare W32 (IGE SN)
    0x4F -> Just $ ICompare W32 (IGE US)

    0x50 -> Just $ IUnary W64 IEQZ
    0x51 -> Just $ ICompare W64 IEQ
    0x52 -> Just $ ICompare W64 INE
    0x53 -> Just $ ICompare W64 (ILT SN)
    0x54 -> Just $ ICompare W64 (ILT US)
    0x55 -> Just $ ICompare W64 (IGT SN)
    0x56 -> Just $ ICompare W64 (IGT US)
    0x57 -> Just $ ICompare W64 (ILE SN)
    0x58 -> Just $ ICompare W64 (ILE US)
    0x59 -> Just $ ICompare W64 (IGE SN)
    0x5A -> Just $ ICompare W64 (IGE US)

    0x5B -> Just $ FCompare W32 FEQ
    0x5C -> Just $ FCompare W32 FNE
    0x5D -> Just $ FCompare W32 FLT
    0x5E -> Just $ FCompare W32 FGT
    0x5F -> Just $ FCompare W32 FLE
    0x60 -> Just $ FCompare W32 FGE

    0x61 -> Just $ FCompare W64 FEQ
    0x62 -> Just $ FCompare W64 FNE
    0x63 -> Just $ FCompare W64 FLT
    0x64 -> Just $ FCompare W64 FGT
    0x65 -> Just $ FCompare W64 FLE
    0x66 -> Just $ FCompare W64 FGE

    0x67 -> Just $ IUnary W32 IClz
    0x68 -> Just $ IUnary W32 ICtz
    0x69 -> Just $ IUnary W32 IPopCnt

    0x6A -> Just $ IBinary W32 IAdd
    0x6B -> Just $ IBinary W32 ISub
    0x6C -> Just $ IBinary W32 IMul
    0x6D -> Just $ IBinary W32 (IDiv SN)
    0x6E -> Just $ IBinary W32 (IDiv US)
    0x6F -> Just $ IBinary W32 (IRem SN)
    0x70 -> Just $ IBinary W32 (IRem US)

    0x71 -> Just $ IBinary W32 IAnd
    0x72 -> Just $ IBinary W32 IOr
    0x73 -> Just $ IBinary W32 IXOr
    0x74 -> Just $ IBinary W32 IShiftL
    0x75 -> Just $ IBinary W32 (IShiftR SN)
    0x76 -> Just $ IBinary W32 (IShiftR US)
    0x77 -> Just $ IBinary W32 IRotL
    0x78 -> Just $ IBinary W32 IRotR

    0x79 -> Just $ IUnary W64 IClz
    0x7A -> Just $ IUnary W64 ICtz
    0x7B -> Just $ IUnary W64 IPopCnt

    0x7C -> Just $ IBinary W64 IAdd
    0x7D -> Just $ IBinary W64 ISub
    0x7E -> Just $ IBinary W64 IMul
    0x7F -> Just $ IBinary W64 (IDiv SN)
    0x80 -> Just $ IBinary W64 (IDiv US)
    0x81 -> Just $ IBinary W64 (IRem SN)
    0x82 -> Just $ IBinary W64 (IRem US)

    0x83 -> Just $ IBinary W64 IAnd
    0x84 -> Just $ IBinary W64 IOr
    0x85 -> Just $ IBinary W64 IXOr
    0x86 -> Just $ IBinary W64 IShiftL
    0x87 -> Just $ IBinary W64 (IShiftR SN)
    0x88 -> Just $ IBinary W64 (IShiftR US)
    0x89 -> Just $ IBinary W64 IRotL
    0x8A -> Just $ IBinary W64 IRotR

    0x8B -> Just $ FUnary W32 FAbs
    0x8C -> Just $ FUnary W32 FNeg
    0x8D -> Just $ FUnary W32 FCeil
    0x8E -> Just $ FUnary W32 FFloor
    0x8F -> Just $ FUnary W32 FTrunc
    0x90 -> Just $ FUnary W32 FNearest
    0x91 -> Just $ FUnary W32 FSqrt

    0x92 -> Just $ FBinary W32 FAdd
    0x93 -> Just $ FBinary W32 FSub
    0x94 -> Just $ FBinary W32 FMul
    0x95 -> Just $ FBinary W32 FDiv
    0x96 -> Just $ FBinary W32 FMin
    0x97 -> Just $ FBinary W32 FMax
    0x98 -> Just $ FBinary W32 FCopySign

    0x99 -> Just $ FUnary W64 FAbs
    0x9A -> Just $ FUnary W64 FNeg
    0x9B -> Just $ FUnary W64 FCeil
    0x9C -> Just $ FUnary W64 FFloor
    0x9D -> Just $ FUnary W64 FTrunc
    0x9E -> Just $ FUnary W64 FNearest
    0x9F -> Just $ FUnary W64 FSqrt

    0xA0 -> Just $ FBinary W64 FAdd
    0xA1 -> Just $ FBinary W64 FSub
    0xA2 -> Just $ FBinary W64 FMul
    0xA3 -> Just $ FBinary W64 FDiv
    0xA4 -> Just $ FBinary W64 FMin
    0xA5 -> Just $ FBinary W64 FMax
    0xA6 -> Just $ FBinary W64 FCopySign

    0xA7 -> Just $ WrapI64
    0xA8 -> Just $ IntTruncFlt W32 W32 SN
    0xA9 -> Just $ IntTruncFlt W32 W32 US
    0xAA -> Just $ IntTruncFlt W32 W64 SN
    0xAB -> Just $ IntTruncFlt W32 W64 US
    0xAC -> Just $ ExtendI32 SN
    0xAD -> Just $ ExtendI32 US
    0xAE -> Just $ IntTruncFlt W64 W32 SN
    0xAF -> Just $ IntTruncFlt W64 W32 US
    0xB0 -> Just $ IntTruncFlt W64 W64 SN
    0xB1 -> Just $ IntTruncFlt W64 W64 US
    0xB2 -> Just $ Flt2Int W32 W32 SN
    0xB3 -> Just $ Flt2Int W32 W32 US
    0xB4 -> Just $ Flt2Int W32 W64 SN
    0xB5 -> Just $ Flt2Int W32 W64 US
    0xB6 -> Just $ FltDemote
    0xB7 -> Just $ Flt2Int W64 W32 SN
    0xB8 -> Just $ Flt2Int W64 W32 US
    0xB9 -> Just $ Flt2Int W64 W64 SN
    0xBA -> Just $ Flt2Int W64 W64 US
    0xBB -> Just $ FltPromote
    0xBC -> Just $ Reinterpret (NumInt, W32) (NumFlt, W32)
    0xBD -> Just $ Reinterpret (NumInt, W64) (NumFlt, W64)
    0xBE -> Just $ Reinterpret (NumFlt, W32) (NumFlt, W32)
    0xBF -> Just $ Reinterpret (NumFlt, W64) (NumFlt, W64)
    _ -> Nothing

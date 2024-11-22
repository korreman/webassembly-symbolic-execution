module Assertions where

import Data.SBV hiding (Predicate, label)

import Control.Arrow ((>>>))
import Data.Functor ((<&>))
import Data.Function ((&))
import Control.Applicative (liftA2)

import qualified Data.ByteString as B
import Data.SBV.Tools.Overflow
import qualified Data.Vector as V

import Machine
import SValue
import AST

parseAsserts :: [String] -> Predicate
parseAsserts = map assertLookup >>> foldl1 combinePredicates

assertLookup :: String -> Predicate
assertLookup "none" = assertTrue
assertLookup "unreachable" = assertUnreachable
assertLookup "intDivZero" = assertIntDivZero
assertLookup str = error ("unknown assertion: " ++ str)

combinePredicates :: Predicate -> Predicate -> Predicate
combinePredicates =
  liftA2 $ \ (b1, sb1) (b2, sb2) ->
  (b1 || b2, (literal b1 .&& sb1) .|| (literal b2 .&& sb2))

-- | This term is never satisfied
assertTrue :: Predicate
assertTrue = const (False, sFalse)

-- TODO: Add addition, subtraction, division, etc.
overflowTerm :: Predicate
overflowTerm =
  (,) <$> (
    frames >>> head >>> contexts >>> head >>> code >>> V.head >>> \ instr ->
    instr == IBinary W32 IMul
  ) <*> (
    frames >>> head >>> contexts >>> head >>> stack >>> \ (op2:op1:_) ->
    case (op1, op2) of
      (SW32 x, SW32 y) -> snd $ bvMulO x y
      _ -> sFalse
  )

-- | This term is satisfied if an `unreachable` instruction is encountered
assertUnreachable :: Predicate
assertUnreachable =
  frames >>> head >>> contexts >>> head >>> code >>> V.head >>> \ instr ->
  (instr == Unreachable, sTrue)

-- | This term is satisfied if a division or modulus is performed where the
-- denominator is zero
assertIntDivZero :: Predicate
assertIntDivZero =
  (,) <$> (
    frames >>> head >>> contexts >>> head >>> code >>> V.head >>> \ instr ->
    case instr of
      IBinary _ (IDiv _) -> True
      IBinary _ (IRem _) -> True
      _ -> False
  ) <*> (
    frames >>> head >>> contexts >>> head >>> stack >>> \ (v:_) ->
    case v of
      SW32 x -> x .== 0
      SW64 x -> x .== 0
      _ -> sFalse
  )

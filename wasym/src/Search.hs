module Search (searchFunc, evalMachine, makeConfig, makeVars, findSat) where

import qualified Data.Vector as V
import Data.Function ((&))
import Data.Functor ((<&>))
import Control.Applicative ((<|>), empty)

import Data.SBV hiding (Predicate)
import Data.SBV.Trans.Control

import Control.Monad.Trans
import Control.Monad.State.Lazy
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe

import AST
import Machine
import Exec
import SValue

type Result = String

--------------------------------------------------------------------------------
-- * Program execution
--------------------------------------------------------------------------------

searchFunc :: Idx -> Machine Result
searchFunc funcIdx =
  asks symbols >>= \ syms ->
  invoke syms funcIdx *> findSat

-- | Creates a config
makeConfig :: Module -> Idx  -> Predicate -> Symbolic Config
makeConfig mdl funcIdx searchTerm =
  let argTypes = funcParams . fType $ funcs mdl V.! fromIntegral funcIdx in
  makeVars argTypes <&> \ syms ->
  Config {
    wasmModule = mdl,
    maxDepth = Just 1000,
    predicate = searchTerm,
    symbols = syms
  }

-- | Unpacks the machine stack
evalMachine :: Machine a -> WASMState -> Config -> Symbolic (Maybe a)
evalMachine machine st =
  query .
  runMaybeT .
  (`evalStateT` st) .
  runReaderT (
    machine
  )

-- | Creates a list of symbolic variables given their names
makeVars :: Vec ValType -> Symbolic (Vec SValue)
makeVars types =
  setOption (ProduceUnsatCores True) *>
  (V.map mkSValue types & sequenceA)

--------------------------------------------------------------------------------
-- * State space search
--------------------------------------------------------------------------------

-- | Attempts to find input values which will eventually put a WASM machine into
-- a state satisfying the given predicate.
findSat :: Machine Result
findSat =
  -- Apply the predicate to our state and push the resulting constraint
  asks predicate <*> get >>= \ (concretePred, symbolicPred) ->
  if not concretePred then handleBranch else
  pushConstraint symbolicPred *>
  -- Query the SMT solver about the symbolic value
  checkPath >>= \ satPred ->
  if satPred
    -- If satisfied, return the assignments
    then getResult
    -- If not, move forward
    else popConstraint *> handleBranch

-- | Handles searching branches if necessary, otherwise just exec
handleBranch :: Machine Result
handleBranch =
  -- If a maximum depth is reached, stop the search
  gets depth >>= \ d ->
  asks maxDepth >>= \ maxd ->
  if maxd == Just d
    then {--mPrint "maximum depth reached" *>--} empty
    else modify (\ cfg -> cfg { depth = d + 1 }) *>

  popInstr >>= \ instr ->
  --mPrint (show instr) *>
  case instr of
    BranchIf n ->
      popVal >>= \ v ->
      case v of
        SW32 x ->
          (
            ( pushConstraint (x .== 0) *>
              guardPath *>
              execInstr Nop *>
              findSat
            ) &
            mapQ (<* pop 1)
          ) <|> (
            ( pushConstraint (x ./= 0) *>
              guardPath *>
              execInstr (Branch n) *>
              findSat
            ) &
            mapQ (<* pop 1)
          )
        _ -> error "attempt to branch on value that isn't i32"
    If res body1 body2 ->
      popVal >>= \ v ->
      case v of
        SW32 x ->
          (
            ( pushConstraint (x ./= 0) *>
              guardPath *>
              execInstr (Block res body1) *>
              findSat
            ) &
            mapQ (<* pop 1)
          ) <|> (
            ( pushConstraint (x .== 0) *>
              guardPath *>
              execInstr (Block res body2) *>
              findSat
            ) &
            mapQ (<* pop 1)
          )
        _ -> error "attempt to branch on value that isn't i32"
    _ -> execInstr instr *> findSat

-- | Checks the current constraints for satisfiability and nullifies this
-- execution path if it isn't feasible.
guardPath :: Machine ()
guardPath =
  checkPath >>= \ satPath ->
  if not satPath
    then (getUnsatCore >>= traverse mPrint) *>
         --mPrint "unsatisfiable branch" *>
         empty
    else pure ()

getResult :: Machine Result
getResult =
  asks symbols >>= sequenceA . V.map getSValue <&> show

getSValue :: SValue -> Machine Value
getSValue (SW32 x) = I32Val <$> getValue x
getSValue (SW64 x) = I64Val <$> getValue x
getSValue (SF32 x) = F32Val <$> getValue x
getSValue (SF64 x) = F64Val <$> getValue x

checkPath :: Machine Bool
checkPath =
  checkSat <&> \ res -> case res of
    Sat -> True
    Unsat -> False
    Unk -> error "Satisfiability unknown."

pushConstraint :: SBool -> Machine ()
pushConstraint c =
  push 1 *> liftQ (constrain c)

popConstraint :: Machine ()
popConstraint = pop 1

mapQ :: (Query (Maybe (a, WASMState)) -> Query (Maybe (b, WASMState))) ->
        Machine a -> Machine b
mapQ = mapReaderT . mapStateT . mapMaybeT

liftQ :: Query a -> Machine a
liftQ = lift . lift . lift

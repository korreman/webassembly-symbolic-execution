module Exec where

import Data.Function ((&))
import Data.Functor ((<&>))
import Control.Applicative
import Control.Arrow ((>>>))

import Data.Maybe (isJust)
import qualified Data.Vector as V

import Control.Monad.State
import Control.Monad.Reader

import Data.SBV

import AST
import Machine
import SValue

-- | Ends finished blocks and returns from finished functions
execFallthrough :: Machine ()
execFallthrough =
  -- Get the contexts and top context
  gets frames >>= \ frames ->
  -- Block the execution path if no frames left on stack
  -- TODO: maybe put in `guardPath`?
  if length frames == 0 then empty else
  let ctxs = contexts (head frames) in
  let ctx = head ctxs in
  -- If the code for the current context has fully executed
  if V.length (code ctx) > 0 then pure () else
  (
    -- If there is only this context, return from the function
    if length ctxs == 1 then execInstr Return else
    -- Else, end the block without executing the label WRONG
    popContext <&> stack >>= \ st ->
    case st of
      [x] -> pushVal x
      [] -> pure ()
      _ -> error "stack too large when exiting block"
  ) *>
  -- Repeat in case there are more fallthroughs to perform
  execFallthrough

execInstr :: Instr -> Machine ()
execInstr instr =
  execInstr' instr *> execFallthrough

execInstr' :: Instr -> Machine ()
execInstr' instr = case instr of
  Nop -> pure ()

  Unreachable -> error "Unreachable instruction was reached"

  Drop -> () <$ popVal

-- | Pushes the specified value to the stack.
  Constant v ->
    case v of
      I32Val x -> pushVal . SW32 . literal $ x
      I64Val x -> pushVal . SW64 . literal $ x
      F32Val x -> pushVal . SF32 . literal $ x
      F64Val x -> pushVal . SF64 . literal $ x

-- | Pops a value, performs a unary operation, pushes the result.
  --IUnary tp op -> popVal >>= (evalIUnOp op >>> pushVal)

-- | Pops two values and performs a binary operation on them. Note that the
-- first operand is the second value to be popped!
  IBinary _ op -> (flip (evalIBinOp op) <$> popVal <*> popVal) >>= pushVal

-- | Pops two values and performs a comparison operation between two integers.
  ICompare _ op -> (flip (evalIRelOp op) <$> popVal <*> popVal) >>= pushVal

  FBinary _ op -> (flip (evalFBinOp op) <$> popVal <*> popVal) >>= pushVal

-- | Reads the specified local value and pushes it to the stack.
  LocalGet idx -> (peekVars <&> (V.! (fromIntegral idx))) >>= pushVal

-- | Pops a value and writes it to the specified local.
  LocalSet idx -> popVal >>= \ v -> modifyVars (V.// [(fromIntegral idx, v)])

-- | Peeks at the top value and writes it to the specified local.
  LocalTee idx -> peekVal >>= \ v -> modifyVars (V.// [(fromIntegral idx, v)])

-- | Constructs a new context and pushes it to the top
  Block res body ->
    pushContext $ Context {
      code = body,
      stack = empty,
      ctxLabel = Nop,
      arity = isJust res
    }

-- | Constructs a new context and pushes it to the top. The label is set to
-- another loop
  Loop res body ->
    pushContext $ Context {
      code = body,
      stack = empty,
      ctxLabel = Loop res body,
      arity = False
    }

  Branch n ->
    let idx = fromIntegral n in
    -- Create an action that will pop n + 1 contexts
    let popContexts = sequenceA $ replicate (idx + 1) popContext in
    -- Retrieve the last context to pop (inefficient lookup)
    -- Maybe we could pop the contexts into a vector to avoid the lookahead
    peekFrame <&> contexts <&> (!! idx) >>= \ ctx ->
    -- If the last context has a result value, pop a value, pop the contexts,
    -- push the value back on the stack
    if arity ctx
      then popVal <* popContexts <* execInstr (ctxLabel ctx) >>= pushVal
      else popContexts *> execInstr (ctxLabel ctx)

  Call idx ->
    -- Retrieve function by index, unpack
    asks (wasmModule >>> funcs >>> (V.! fromIntegral idx)) >>=
    \ Func {fType = FuncType params res, locals = lcs, body = bd} ->
    -- Pop values corresponding to the number of arguments
    sequenceA (V.replicate (V.length params) popVal) <&>
    -- Reverse and concatenate to n zero-values for every local
    V.reverse <&> (V.++ (mkZero <$> lcs)) >>= \ newVars ->
    -- Push a frame with the locals.
    pushFrame ( Frame {vars = newVars, contexts = [], res = res} ) *>
    -- Push a context with body and return type of the function
    execInstr (Block res bd)

  Return ->
    peekFrame <&> res >>= \ res ->
    if isJust res
      then popVal <* popFrame >>= pushVal
      else () <$ popFrame

  Load valType (MemArg offset _) ->
    case valType of
      (NumInt, W32) -> 4
      (NumInt, W64) -> 8
      _ -> error "float memory operations unsupported"
    & \ numBytes ->
    popVal >>= \ v ->
    gets memory >>= \ mem ->
    case v of
      SW32 addr ->
        vFromBytes valType (
          readArray mem <$>
          [addr + literal offset .. numBytes + addr + literal offset - 1]
        )
        & pushVal
      _ -> error "popped value wasn't an address"

  Store valType (MemArg offset _) ->
    case valType of
      (NumInt, W32) -> 4
      (NumInt, W64) -> 8
      _ -> error "float memory operations unsupported"
    & \ numBytes ->
    popVal >>= \ addr ->
    popVal <&> vToBytes >>= \ valBytes ->
    let writeFunc addr byte mem = writeArray mem addr byte in
    case addr of
      SW32 addr ->
        let writeToMem = foldl (.) id $ zipWith writeFunc [addr + literal offset ..] valBytes
        in modify (\ st @ (WASMState {memory = mem}) -> st {memory = writeToMem mem})
      _ -> error "popped value wasn't an address"

  _ -> error $ "unsupported instruction: " ++ show instr


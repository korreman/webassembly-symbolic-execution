module Machine where

import Data.Functor ((<&>))
import Control.Arrow ((>>>))
import Control.Applicative

import Data.Word (Word64)
import Data.Maybe (isJust)
import qualified Data.Vector as V

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Trans.Maybe

import Data.SBV hiding (Predicate)
import Data.SBV.Control

import AST
import SValue

type Predicate = WASMState -> (Bool, SBool)
type Precondition = Config -> SBool

--------------------------------------------------------------------------------
-- * Configuration and runtime state
--------------------------------------------------------------------------------

type Machine a = ReaderT Config (StateT WASMState (MaybeT Query)) a

data Config = Config {
  wasmModule :: Module,
  symbols :: Vec SValue,
  maxDepth :: Maybe Word64,
  predicate :: Predicate
}

type Stack a = [a]
type Vars = Vec SValue

data WASMState = WASMState {
  frames :: Stack Frame,
  memory :: SArray Word32 Word8,
  depth :: Word64
}

data Frame = Frame {
  vars :: Vec SValue,
  contexts :: Stack Context,
  res :: Maybe ValType
}

data Context = Context {
  code :: Expr,
  stack :: Stack SValue,
  ctxLabel :: Instr,
  arity :: Bool
}

startState :: Symbolic WASMState
startState =
  newArray "memory" (Just 0) <&> \ mem ->
  WASMState {
    frames = [],
    memory = mem,
    depth = 0
  }

initMemory :: Int -> WASMState -> Symbolic WASMState
initMemory numBytes st@(WASMState {memory = mem}) =
  mkFreeVars numBytes <&> \ symBytes ->
  let writeFunc addr val mem = writeArray mem addr val in
  let modFunc = foldl (.) id $ zipWith writeFunc symBytes [0..] in
  st {memory = modFunc mem}

--------------------------------------------------------------------------------
-- * Machine operations
--------------------------------------------------------------------------------

-- Invokes a specific function. Can be called without prior setup
invoke :: Vars -> Idx -> Machine ()
invoke args idx =
  asks (wasmModule >>> funcs >>> (V.! fromIntegral idx)) >>=
  \ Func {fType = FuncType _ res, locals = lcs, body = bd} ->
  let frameVars = args <|> (mkZero <$> lcs) in
  let frameCtx =
       Context {
         code = bd,
         stack = empty,
         ctxLabel = Nop,
         arity = isJust res
       } in
  pushFrame $
  Frame {
    vars = frameVars,
    contexts = pure frameCtx,
    res = res
  }

-- | Prints a message using IO
mPrint :: String -> Machine ()
mPrint msg =
  lift . lift . lift . io $ putStrLn msg

-- | Retrieve the top frame from the execution stack
peekFrame :: Machine Frame
peekFrame =
  gets frames <&> \ fs ->
  case fs of
    [] -> error "Attempt to peek frame from empty stack"
    f:_ -> f

-- | Pop off the top frame from the execution stack and return it
popFrame :: Machine Frame
popFrame =
  state $ \ ws ->
  case frames ws of
    (f:fs) -> (f, ws {frames = fs})
    _ -> error "Attempt to pop frame from empty stack"

-- | Push a new frame to the execution stack
pushFrame :: Frame -> Machine ()
pushFrame f =
  modify $ \ ws -> ws {frames = f : (frames ws)}

-- | Modify the current top frame on the execution stack
modifyFrame :: (Frame -> Frame) -> Machine ()
modifyFrame fn =
  (popFrame <&> fn) >>= pushFrame

-- | Push a context to the execution stack
pushContext :: Context -> Machine ()
pushContext ctx =
  modifyFrame $ \ frame ->
  frame {contexts = ctx : (contexts frame)}

 -- | Pop the top context on the top frame.
popContext :: Machine Context
popContext =
  popFrame >>= \ frame ->
  case contexts frame of
    [] -> empty
    ctx : rest ->
      (pushFrame $ frame {contexts = rest}) <&> const ctx

-- | Retrieve the top context on the top frame.
peekContext :: Machine Context
peekContext =
  popContext >>= \ ctx -> pushContext ctx <&> const ctx

-- | Pop an instruction from the code of the top context, on the top frame.
popInstr :: Machine Instr
popInstr =
  popContext >>= \ ctx ->
  case unconsV (code ctx) of
    Nothing -> error "Attempt to pop instruction from empty expr"
    Just (instr, expr) -> pushContext (ctx {code = expr}) *> pure instr

-- | Push a value, to the stack of the top context, on the top frame.
pushVal :: SValue -> Machine ()
pushVal v =
  (popContext <&> \ ctx -> ctx {stack = v : (stack ctx)}) >>= pushContext

-- | Pop a value, from stack of the top context, on the top frame.
popVal :: Machine SValue
popVal =
  popContext >>= \ ctx ->
  case stack ctx of
    [] -> empty
    v:rest -> (pushContext $ ctx {stack = rest}) <&> const v

-- | Retrieve the current value from the top of the stack
peekVal :: Machine SValue
peekVal =
  peekContext >>= \ ctx ->
  case stack ctx of
    [] -> empty
    v:_ -> pure v

-- | Retrieve the local values of the top frame.
peekVars :: Machine (Vec SValue)
peekVars =
  peekFrame <&> vars

-- | Modify the local values of the top frame with a given function.
modifyVars :: (Vec SValue -> Vec SValue) -> Machine ()
modifyVars f =
  modifyFrame (\ frame -> frame {vars = f $ vars frame})


import ParserWASM
import AST
import Assertions
import Machine
import Search

import Data.Word (Word64)
import Data.Function ((&))
import Data.SBV (z3, runSMTWith)
import System.Environment (getArgs)
import qualified Data.ByteString as BS
import Text.Read (readMaybe)

usageString :: String
usageString = "\
\usage: wasym MODULE FUNCTION DEPTH_LIMIT [ASSERTION+]\
\\n\
\\nlimit can be either \'none\' or an integer\
\\npossible assertions:\
\\n    none\
\\n    unreachable\
\\n    intDivZero"

main :: IO ()
main = do
  args <- getArgs
  case args of
    modulePath : funcName : limit : asserts -> do
      mdl <- readModule modulePath
      let funcIdx = getFunc mdl funcName
      let parsedLimit = readMaybe limit
      let assertion = parseAsserts asserts
      case funcIdx of
        Nothing -> putStrLn ("function not found:'" ++ funcName ++ "'")
        Just idx -> execute mdl idx parsedLimit assertion
    _ -> putStrLn usageString

execute :: Module -> Idx -> Maybe Word64 -> Predicate -> IO ()
execute mdl funcIdx limit p =
    (
      makeConfig mdl funcIdx p >>= \ cfg ->
      startState >>= \ st ->
      evalMachine (searchFunc funcIdx) st (cfg {maxDepth = limit})
    )
    & runSMTWith z3 >>= \ smtRes -> case smtRes of
      Nothing -> putStrLn "assertions held"
      Just ass ->
        putStrLn "assertions failed with the following value assignments:" *>
        putStrLn ass

readModule :: String -> IO Module
readModule filename = do
  file <- BS.readFile filename
  let parseResult = parseWASM filename file
  case parseResult of
    Left err -> error err
    Right m -> pure m

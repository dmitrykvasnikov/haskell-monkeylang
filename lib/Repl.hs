module Repl where

import           Control.Monad.Trans.State.Strict (evalStateT, execState)
import           Evaluator                        as E
import           Lexer
import           Parser
import           System.IO
import           Types.Ast
import           Types.Object                     (Object)
import           Types.Token                      (Token)

prompt :: String
prompt = ">>> "

eval :: IO ()
eval = do
  putStr prompt
  str <- getLine
  case str of
    "exit" -> putStrLn "GoodBye!"
    _ -> do
      case take 3 str == ":l " of
        True  -> readFile (drop 3 str) >>= printProgram >> eval
        False -> printProgram str >> eval

printProgram :: String -> IO ()
printProgram src = do
  let sts = evalStateT parseProgram (mkInput src)
  case sts of
    Left e -> putStrLn $ show e
    Right r -> case evalProgram r of
      Left e' -> putStrLn $ show e'
      Right o -> putStrLn $ show o

--   putStrLn
--     . ( \sts' -> case sts' of
--           Right (BLOCK sts'') -> show . map show $ evalProgram sts''
--           Left e              -> show e
--       )

repl :: IO ()
repl = do
  hSetBuffering stdout NoBuffering
  putStrLn "Welcome to Monkey REPL in Haskell!"
  putStrLn " 'exit'             for exit"
  putStrLn " ':l filename'      to load and run code from file"
  putStrLn " 'exp1; expr2; ...' to evaluate list of expressions"
  eval

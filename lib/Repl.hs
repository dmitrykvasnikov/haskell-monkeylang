module Repl where

import           Control.Monad.Trans.State.Strict (evalStateT)
import           Lexer
import           Parser
import           System.IO

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
  putStrLn
    . ( \sts' -> case sts' of
          Right r -> show r
          Left e  -> show e
      )
    $ sts

repl :: IO ()
repl = do
  hSetBuffering stdout NoBuffering
  putStrLn "Welcome to Monkey REPL in Haskell!"
  eval

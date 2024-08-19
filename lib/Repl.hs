module Repl where

import           Control.Monad.Trans.State.Strict (evalStateT)
import           Lexer
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
      let tokens = evalStateT getTokens (mkInput str)
      mapM_
        (putStrLn)
        ( ( \t -> case t of
              Right t' -> map show t'
              Left e'  -> [show e']
          )
            tokens
        )
      eval

repl :: IO ()
repl = do
  hSetBuffering stdout NoBuffering
  putStrLn "Welcome to Monkey REPL in Haskell!"
  eval

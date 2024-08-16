module Repl where

import           Control.Monad.State.Strict
import           Lexer

prompt :: String
prompt = ">>"

repl :: IO ()
repl = do
  putStr prompt
  i <- getLine
  if i == "exit"
    then putStrLn "Bye!"
    else do
      let toks = evalState lexer (mkLInputFs i)
      putStrLn . show $ toks
      repl

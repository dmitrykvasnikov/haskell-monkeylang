module Main where

import           Constant
import           Lexer
import           Repl
import           System.IO
import           Token

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Welcome to Monkey REPL in Haskell!\n"
  repl

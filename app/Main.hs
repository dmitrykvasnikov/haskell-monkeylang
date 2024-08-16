module Main where

import           AST
import           Constant
import           Control.Monad.State.Strict
import           Lexer
import           Parser
import           Repl
import           System.IO
import           Test
import           Token

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Welcome to Monkey REPL in Haskell!\n"
  repl

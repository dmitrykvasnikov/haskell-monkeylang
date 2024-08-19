module Main where

import           Control.Monad.Trans.State.Strict
import           Lexer
import           System.IO
import           Types.Token

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Welcome to Monkey REPL in Haskell!\n"

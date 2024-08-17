module Main where

import           System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Welcome to Monkey REPL in Haskell!\n"

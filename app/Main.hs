module Main where

import           Control.Monad.State.Strict
import           Lexer
import           Token

main :: IO ()
main = do
  i <- mkInput <$> readFile "input"
  putStrLn . show $ runState lexer i

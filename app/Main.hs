module Main where

import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State.Strict
import           Input
import           Lexer
import           Repl

main :: IO ()
main = repl

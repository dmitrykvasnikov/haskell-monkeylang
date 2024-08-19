module Main where

import           Control.Monad.Trans.State.Strict
import           Lexer
import           Repl
import           System.IO
import           Types.Token

main :: IO ()
main = repl

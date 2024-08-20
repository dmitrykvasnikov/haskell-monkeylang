module Main where

import           Control.Monad.Trans.State.Strict
import           Lexer
import           Parser
import           Repl
import           System.IO
import           Types.Token

main :: IO ()
main = repl

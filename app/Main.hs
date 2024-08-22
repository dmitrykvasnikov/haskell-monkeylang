module Main where

import           Control.Monad.Trans.State.Strict
import           Lexer
import           Object
import           Parser
import           Repl
import           System.IO
import           Types.Ast
import           Types.Token

main :: IO ()
main = repl

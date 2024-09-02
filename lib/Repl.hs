module Repl where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State.Strict
import           Input
import           Lexer
import           Parser
import           System.Console.Pretty
import           System.IO
import           Types.Ast
import           Types.Error

prompt :: String
prompt = ">>>> "

replTokens :: IO ()
replTokens = do
  liftIO . putStrLn $ "Enter tokens separeated with ';'"
  liftIO . putStr $ prompt
  input <- liftIO getLine
  res <- (evalStateT . runExceptT) getTokens $ makeInput input
  case res of
    Right tokens -> do
      (putStrLn $ color Green "Done!") >> mapM_ (\token -> putStrLn $ "TOKEN: " <> show token) tokens >> putStrLn "" >> replTokens
    Left err -> printError err >> putStrLn "" <> replTokens

replExpressions :: IO ()
replExpressions = do
  liftIO . putStrLn $ "Enter tokens separeated with ';'"
  liftIO . putStr $ prompt
  input <- liftIO getLine
  res <- (evalStateT . runExceptT) parseProgram $ makeInput input
  case res of
    Right (BlockS _ _ exprs) -> do
      (putStrLn $ color Green "Done!") >> mapM_ (\expr -> putStrLn $ "EXPRESSION: " <> show expr) exprs >> putStrLn "" >> replExpressions
    Left err -> printError err >> putStrLn "" <> replExpressions

printError :: Error -> IO ()
printError InternalError = putStrLn $ color Yellow "Internal error"
printError (LexerError (l, c) msg src) = do
  putStrLn (color Yellow "Error")
  putStrLn $ "Lexer error at line " <> show l <> ", position " <> show c <> "\n" <> msg <> "\nSource code:\n" <> (take (c - 1) src <> color Red (drop (c - 1) src)) <> "\n"
printError (ParserError (l, c) msg src) = do
  putStrLn (color Yellow "Error")
  putStrLn $ "Parser error at line " <> show l <> ", position " <> show c <> "\n" <> msg <> "\nSource code:\n" <> (take (c - 1) src <> color Red (drop (c - 1) src)) <> "\n"

repl :: IO ()
repl = do
  hSetBuffering stdout NoBuffering
  putStrLn "Welcome to MonkeyLand simple interpreter"
  replExpressions

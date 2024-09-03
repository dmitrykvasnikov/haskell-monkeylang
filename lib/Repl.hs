module Repl where

import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State.Strict
import           Data.HashMap.Internal.Array      (update)
import qualified Data.Text                        as T
import           Eval
import           Input
import           Lexer
import           Parser
import           System.Console.Pretty
import           System.Directory
import           System.IO
import           Types.Error
import           Types.Object

prompt :: String
prompt = ">>>> "

replLexer :: IO ()
replLexer = do
  putStrLn $ "Enter tokens separeated with ';'"
  putStr $ prompt
  input <- getLine
  res <- (evalStateT . runExceptT) getTokens $ makeInput input
  case res of
    Right tokens -> do
      (putStrLn $ color Green "Done!") >> mapM_ (\token -> putStrLn $ "TOKEN: " <> show token) tokens >> putStrLn "" >> replLexer
    Left err -> printError err >> putStrLn "" <> replLexer

replParser :: IO ()
replParser = do
  putStrLn $ "Enter tokens separeated with ';'"
  putStr $ prompt
  input <- getLine
  (r, i) <- (runStateT . runExceptT) parseProgram $ makeInput input
  case r of
    Right _ -> (putStrLn $ color Green "Done!") >> mapM_ (\expr -> putStrLn $ "EXPRESSION: " <> show expr) (program i) >> putStrLn "" >> replParser
    Left err -> printError err >> putStrLn "" <> replParser

replEval :: Input -> IO ()
replEval i = do
  s <- getInput
  parse <- (runStateT . runExceptT) parseProgram $ updateInput s i
  case parse of
    (Left err, _) -> (printError err) >> putStrLn "" >> replEval i
    (Right _, i) -> do
      res <- (runStateT . runExceptT) evalProgram i
      case res of
        (Left err, _)   -> printError err >> putStrLn "" >> replEval i
        (Right obj, i') -> (putStrLn $ color Green $ show obj) >> replEval i'

printError :: Error -> IO ()
printError InternalError = putStrLn $ color Yellow "Internal error"
printError (LexerError (l, c) msg src) = do
  putStrLn (color Yellow "Error")
  putStrLn $ "Lexer error at line " <> show l <> ", position " <> show c <> "\n" <> msg <> "\nSource code:\n" <> (take (c - 1) src <> color Red (drop (c - 1) src)) <> "\n"
printError (ParserError (l, c) msg src) = do
  putStrLn (color Yellow "Error")
  putStrLn $ "Parser error at line " <> show l <> ", position " <> show c <> "\n" <> msg <> "\nSource code:\n" <> (take (c - 1) src <> color Red (drop (c - 1) src)) <> "\n"
printError (EvalError msg src) = do
  putStrLn (color Yellow "Error")
  putStrLn $ "Evaluation error: " <> msg <> "\nSource code:\n" <> color Red src

getInput :: IO String
getInput = do
  putStrLn $ "Enter expressions separeated with ';' or :l 'filename'"
  putStr $ prompt
  i <- getLine
  if take 2 i == ":l"
    then do
      b <- doesFileExist (drop 3 i)
      if b
        then readFile (drop 3 i) >>= return
        else (putStrLn $ color Red "file doesn't exist, try again\n") >> getInput
    else return i

repl :: IO ()
repl = do
  hSetBuffering stdout NoBuffering
  putStrLn "Welcome to MonkeyLand simple interpreter"
  replEval $ makeInput ""

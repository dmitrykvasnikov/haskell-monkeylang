module Repl where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State.Strict
import qualified Data.Text                        as T
import           Eval
import           Input
import           Lexer
import           Parser
import           System.Console.Pretty
import           System.Directory
import           System.IO
import           Types.Ast
import           Types.Error
import           Types.Object
import           Types.Object                     (initialEnv)

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
  res <- (evalStateT . runExceptT) parseProgram $ makeInput input
  case res of
    Right (BlockS _ _ exprs) -> do
      (putStrLn $ color Green "Done!") >> mapM_ (\expr -> putStrLn $ "EXPRESSION: " <> show expr) exprs >> putStrLn "" >> replParser
    Left err -> printError err >> putStrLn "" <> replParser

replEval :: Env -> IO ()
replEval env = do
  i <- getInput
  parse <- (runStateT . runExceptT) parseProgram $ makeInput i
  case parse of
    (Left err, _) -> (printError err) >> (putStrLn $ "") >> replEval env
    (Right program, i) -> do
      res <- (runStateT . runExceptT) (evalProgram program) env
      case res of
        (Left err, _) -> (printEvalError err i) >> replEval env
        (Right obj, env') -> (putStrLn $ color Green $ show obj) >> replEval env'

printError :: Error -> IO ()
printError InternalError = putStrLn $ color Yellow "Internal error"
printError (LexerError (l, c) msg src) = do
  putStrLn (color Yellow "Error")
  putStrLn $ "Lexer error at line " <> show l <> ", position " <> show c <> "\n" <> msg <> "\nSource code:\n" <> (take (c - 1) src <> color Red (drop (c - 1) src)) <> "\n"
printError (ParserError (l, c) msg src) = do
  putStrLn (color Yellow "Error")
  putStrLn $ "Parser error at line " <> show l <> ", position " <> show c <> "\n" <> msg <> "\nSource code:\n" <> (take (c - 1) src <> color Red (drop (c - 1) src)) <> "\n"

printEvalError (EvalError b e msg) inp = do
  putStrLn (color Yellow "Error")
  putStrLn $ "Evaluation error: " <> msg <> "\nSource code:\n" <> src
  where
    begin = T.take (e - b) $ T.drop b (input inp)
    end = T.takeWhile (/= '\n') (T.drop e (input inp))
    src = T.unpack (begin <> end)

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
  replEval initialEnv

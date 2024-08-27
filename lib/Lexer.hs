module Lexer (Token, Input, nextToken, runLexer, getTokens) where

import           Control.Applicative              (asum, (<|>))
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State.Strict
import           Data.Char                        (isAlpha, isAlphaNum, isDigit)
import           Data.Maybe                       (fromJust)
import qualified Data.Text                        as T
import           Debug.Trace
import           Input
import           Types.Error
import           Types.Token

runLexer :: Stream a -> String -> IO ((Either Error a), Input)
runLexer l i = (runStateT . runExceptT) l $ makeInput i

getTokens :: Stream [Token]
getTokens = do
  token <- lift . gets $ curToken
  case token of
    EOF     -> return [EOF]
    NOTOKEN -> nextToken >> getTokens
    _       -> nextToken >> getTokens >>= \tokens -> return (token : tokens)

nextToken :: Stream Token
nextToken = do
  skipWhiteSpaces
  token <- asum [doubleCharToken, singleCharToken, intToken, idOrKeywordToken, stringToken]
  lift . modify $ moveInput
  pushToken token
  return token

singleCharToken :: Stream Token
singleCharToken = do
  c <- lift . gets $ curChar
  case c of
    '-'    -> return MINUS
    '\NUL' -> return EOF
    '+'    -> return PLUS
    '/'    -> return DIV
    '*'    -> return MULT
    '('    -> return LPAREN
    ')'    -> return RPAREN
    '{'    -> return LBRACE
    '}'    -> return RBRACE
    '['    -> return LBRACKET
    ']'    -> return RBRACKET
    '\n'   -> return SEMICOLON
    ';'    -> return SEMICOLON
    '='    -> return ASSIGN
    '!'    -> return NOT
    ':'    -> return COLON
    ','    -> return COMMA
    '<'    -> return LST
    '>'    -> return GRT
    _      -> makeLexerError Nothing Nothing

doubleCharToken :: Stream Token
doubleCharToken = do
  l <- lift . gets $ curLine
  c <- lift . gets $ curChar
  pp <- lift . gets $ peekPos
  case (pp < T.length l) of
    True -> case (c, T.index l pp) of
      ('+', '+') -> (lift . modify $ moveInput) >> return CONCAT
      ('=', '=') -> (lift . modify $ moveInput) >> return EQL
      ('!', '=') -> (lift . modify $ moveInput) >> return NOTEQL
      ('<', '>') -> (lift . modify $ moveInput) >> return NOTEQL
      ('<', '=') -> (lift . modify $ moveInput) >> return LSTEQL
      ('=', '<') -> (lift . modify $ moveInput) >> return LSTEQL
      ('=', '>') -> (lift . modify $ moveInput) >> return GRTEQL
      ('>', '=') -> (lift . modify $ moveInput) >> return GRTEQL
      _          -> makeLexerError Nothing Nothing
    False -> makeLexerError Nothing Nothing

intToken :: Stream Token
intToken = do
  c <- lift . gets $ curChar
  case isDigit c of
    True -> peekWhile isDigit >>= \rest -> (checkPeekChar "\n\t\r ;=-+*/]})><!") >> (return . INT $ read @Int (c : rest))
    False -> makeLexerError Nothing Nothing

stringToken :: Stream Token
stringToken = do
  cur <- lift . gets $ curChar
  case cur == '"' of
    True -> do
      str <- peekWhile (\p -> (p /= '"') && (p /= '\n') && (p /= '\NUL'))
      lift . modify $ moveInput
      p <- lift . gets $ curPos
      l <- lift . gets $ line
      c <- lift . gets $ curChar
      if c == '"'
        then (checkPeekChar "\n\t\r ;=-+*/]})><!") >> return (STRING str)
        else makeLexerError (Just (l, p)) (Just "String literal must end with quote '\"'")
    False -> traceShow "sting srr" $ makeLexerError Nothing Nothing

idOrKeywordToken :: Stream Token
idOrKeywordToken = do
  c <- lift . gets $ curChar
  case isAlpha c of
    True -> do
      rest <- peekWhile isAlphaNum
      checkPeekChar "\n\t\r ;=-*+/]})><!"
      let identifier = c : rest
      case lookup identifier keywords of
        (Just keyword) -> return keyword
        Nothing        -> return $ ID identifier
    False -> makeLexerError Nothing Nothing

skipWhiteSpaces :: Stream ()
skipWhiteSpaces = do
  c <- lift . gets $ curChar
  case elem c " \n\t\r" of
    True  -> (lift . modify $ moveInput) >> skipWhiteSpaces
    False -> return ()

-- consume input until condition holds
peekWhile :: (Char -> Bool) -> Stream String
peekWhile cond = do
  pp <- lift . gets $ peekPos
  l <- lift . gets $ curLine
  case (pp < T.length l) of
    True -> do
      let c = T.index l pp
      case cond c of
        True -> (lift . modify $ moveInput) >> peekWhile cond >>= \rest -> return (c : rest)
        False -> return []
    False -> return []

-- check if peekPos has one of chatacters
checkPeekChar :: String -> Stream ()
checkPeekChar pattern = do
  pp <- lift . gets $ peekPos
  src <- lift . gets $ curLine
  case (pp < T.length src) of
    True ->
      if (elem (T.index src pp) pattern)
        then return ()
        else do
          l <- lift . gets $ line
          throwE . LexerError (l, pp) ("unexpected character: '" <> [T.index src pp] <> "'") $ (T.unpack src)
    False -> return ()

makeLexerError :: (Maybe (Int, Int)) -> Maybe String -> Stream Token
makeLexerError mpos merr = do
  c <- lift . gets $ curChar
  l <- lift . gets $ line
  p <- lift . gets $ curPos
  src <- lift . gets $ curLine
  throwE $ LexerError (fromJust (mpos <|> Just (l, p))) (fromJust (merr <|> (Just $ "unexpected character: '" <> [c] <> "'"))) (T.unpack src)

pushToken :: Token -> Stream ()
pushToken token = lift . modify $ (\s -> s {curToken = peekToken s, peekToken = token})

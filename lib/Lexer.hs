module Lexer (Token, Input, nextToken, runLexer, getTokens) where

import           Control.Applicative              (asum, (<|>))
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.Except       (runExceptT, throwE)
import           Control.Monad.Trans.State.Strict (gets, modify, runStateT)
import           Data.Char                        (isAlpha, isAlphaNum, isDigit)
import           Data.Maybe                       (fromJust)
import qualified Data.Text                        as T
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

nextToken :: Stream ()
nextToken = do
  skipWhiteSpaces
  token <- asum [doubleCharToken, singleCharToken, intToken, stringToken, identOrKeywordToken]
  lift . modify $ moveInput
  pushToken token

-- return token

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
  l <- lift . gets $ input
  pp <- lift . gets $ peekPos
  c <- lift . gets $ curChar
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
      (';', ';') -> (lift . modify $ moveInput) >> doubleCharToken
      _          -> makeLexerError Nothing Nothing
    False -> makeLexerError Nothing Nothing

intToken :: Stream Token
intToken = do
  c <- lift . gets $ curChar
  case isDigit c of
    True  -> peekWhile isDigit >>= \rest -> return . INT . read @Int $ c : rest
    False -> makeLexerError Nothing Nothing

stringToken :: Stream Token
stringToken = do
  (l, p) <- lift . gets $ pos
  c <- lift . gets $ curChar
  case c == '"' of
    True -> do
      str <- peekWhile (\s -> s /= '"' && s /= '\n')
      lift . modify $ moveInput
      c' <- lift . gets $ curChar
      if c' == '"'
        then return . STRING $ str
        else makeLexerError (Just (l, p + 1)) (Just "string literal doesn't have closing quote")
    False -> makeLexerError Nothing Nothing

identOrKeywordToken :: Stream Token
identOrKeywordToken = do
  c <- lift . gets $ curChar
  case isAlpha c of
    True -> peekWhile isAlphaNum >>= \rest -> return $ maybe (ID $ c : rest) id (lookup (c : rest) keywords)
    False -> makeLexerError Nothing Nothing

-- combine chars while they satisfy predicate
peekWhile :: (Char -> Bool) -> Stream String
peekWhile cond = do
  pp <- lift . gets $ peekPos
  i <- lift . gets $ input
  case (pp < T.length i) of
    True -> do
      let pc = T.index i pp
      case cond pc of
        True -> (lift . modify $ moveInput) >> peekWhile cond >>= \rest -> return $ pc : rest
        False -> return []
    False -> return []

-- skips all spaces and tabs before next token
skipWhiteSpaces :: Stream ()
skipWhiteSpaces = do
  c <- lift . gets $ curChar
  case elem c " \t\r" of
    True  -> (lift . modify $ moveInput) >> skipWhiteSpaces
    False -> return ()

-- consume input until condition holds
makeLexerError :: (Maybe (Int, Int)) -> Maybe String -> Stream Token
makeLexerError mpos merr = do
  c <- lift . gets $ curChar
  p <- lift . gets $ pos
  s <- lift . gets $ getCurrentLine
  throwE . LexerError (fromJust (mpos <|> Just p)) (fromJust (merr <|> (Just $ "unexpected character: '" <> [c] <> "'"))) $ s

pushToken :: Token -> Stream ()
pushToken token = lift . modify $ (\s -> s {curToken = peekToken s, peekToken = token})

module Lexer where

import           Control.Applicative              (asum, (<|>))
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.Except       (throwE)
import           Control.Monad.Trans.State.Strict (gets, modify)
import           Data.Char                        (isAlpha, isAlphaNum, isDigit)
import           Data.Maybe                       (fromJust)
import qualified Data.Text                        as T
import           Input
import           Types.Error
import           Types.Object
import           Types.Token

-- runLexer :: Lexer a -> String -> IO ((Either Error a), Input)
-- runLexer l i = (runStateT . runExceptT) l $ makeInput i

type Lexer a = Stream Object a

getTokens :: Lexer [Token]
getTokens = do
  token <- fst <$> (lift . gets $ curToken)
  case token of
    EOF     -> return [EOF]
    NOTOKEN -> nextToken >> getTokens
    _       -> nextToken >> getTokens >>= \tokens -> return (token : tokens)

nextToken :: Lexer ()
nextToken = skipWhiteSpaces >> asum [doubleCharToken, singleCharToken, intToken, identOrKeywordToken, stringToken] >>= pushToken >> (lift . modify $ moveInput)

-- return token
singleCharToken :: Lexer Token
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
    '\n'   -> return SEMICOLON_
    ';'    -> return SEMICOLON
    '='    -> return ASSIGN
    '!'    -> return NOT
    ':'    -> return COLON
    ','    -> return COMMA
    '<'    -> return LST
    '>'    -> return GRT
    _      -> makeLexerError Nothing Nothing

doubleCharToken :: Lexer Token
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
      ('&', '&') -> (lift . modify $ moveInput) >> return AND
      ('|', '|') -> (lift . modify $ moveInput) >> return OR
      (';', ';') -> (lift . modify $ moveInput) >> doubleCharToken
      _          -> makeLexerError Nothing Nothing
    False -> makeLexerError Nothing Nothing

intToken :: Lexer Token
intToken = do
  c <- lift . gets $ curChar
  case isDigit c of
    True  -> peekWhile isDigit >>= \rest -> return . INT . read @Int $ c : rest
    False -> makeLexerError Nothing Nothing

stringToken :: Lexer Token
stringToken = do
  (l, p) <- lift . gets $ pos
  c <- lift . gets $ curChar
  case c == '"' of
    True -> do
      str <- peekWhile (not . flip elem "\"\n")
      lift . modify $ moveInput
      c' <- lift . gets $ curChar
      if c' == '"'
        then return . STRING $ str
        else makeLexerError (Just (l, p + 1)) (Just "string literal doesn't have closing quote")
    False -> makeLexerError Nothing Nothing

identOrKeywordToken :: Lexer Token
identOrKeywordToken = do
  c <- lift . gets $ curChar
  case isAlpha c of
    True -> peekWhile isAlphaNum >>= \rest -> return $ maybe (ID $ c : rest) id (lookup (c : rest) keywords)
    False -> makeLexerError Nothing Nothing

-- combine chars while they satisfy predicate
peekWhile :: (Char -> Bool) -> Lexer String
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
skipWhiteSpaces :: Lexer ()
skipWhiteSpaces = do
  c <- lift . gets $ curChar
  case elem c " \t\r" of
    True  -> (lift . modify $ moveInput) >> skipWhiteSpaces
    False -> return ()

-- consume input until condition holds
makeLexerError :: (Maybe (Int, Int)) -> Maybe String -> Lexer Token
makeLexerError mpos merr = do
  c <- lift . gets $ curChar
  p <- lift . gets $ pos
  s <- lift . gets $ currentLine
  throwE . LexerError (fromJust (mpos <|> Just p)) (fromJust (merr <|> (Just $ "unexpected character: '" <> [c] <> "'"))) $ s

pushToken :: Token -> Lexer ()
pushToken token = (lift . gets $ curLinePos) >>= \c -> lift . modify $ (\s -> s {curToken = peekToken s, peekToken = (token, c)})

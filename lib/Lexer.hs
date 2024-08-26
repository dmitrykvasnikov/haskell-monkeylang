module Lexer where

import           Control.Applicative              (asum)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State.Strict
import           Data.Char                        (isAlpha, isAlphaNum, isDigit)
import qualified Data.Text                        as T
import           Input
import           Types.Error
import           Types.Token

runLexer :: Stream a -> String -> IO ((Either Error a), Input)
runLexer l i = (runStateT . runExceptT) l $ makeInput i

getTokens :: Stream [Token]
getTokens = do
  c <- lift . gets $ curChar
  case c of
    '\NUL' -> return []
    ';' -> (lift . modify $ moveInput) >> getTokens
    _ -> nextToken >>= \token -> getTokens >>= \tokens -> return (token : tokens)

nextToken :: Stream Token
nextToken = do
  skipWhiteSpaces
  token <- asum [eofToken, doubleCharToken, singleCharToken, identOrKeywordToken, intToken, stringToken]
  lift . modify $ moveInput
  pushToken token
  return token

eofToken :: Stream Token
eofToken = do
  c <- lift $ gets curChar
  case c of
    '\NUL' -> return EOF
    _      -> makeLexerError

singleCharToken :: Stream Token
singleCharToken = do
  c <- lift . gets $ curChar
  case c of
    '-'  -> return MINUS
    '+'  -> return PLUS
    '/'  -> return DIV
    '*'  -> return MULT
    '('  -> return LPAREN
    ')'  -> return RPAREN
    '{'  -> return LBRACE
    '}'  -> return RBRACE
    '['  -> return LBRACKET
    ']'  -> return RBRACKET
    '\n' -> return SEMICOLON
    ';'  -> return SEMICOLON
    '='  -> return ASSIGN
    '!'  -> return NOT
    ':'  -> return COLON
    ','  -> return COMMA
    '<'  -> return LST
    '>'  -> return GRT
    _    -> makeLexerError

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
      _          -> makeLexerError
    False -> makeLexerError

intToken :: Stream Token
intToken = do
  c <- lift . gets $ curChar
  case isDigit c of
    True -> peekWhile isDigit >>= \rest -> (checkPeekChar "\n\t\r ;=-*/]})><!") >> (return . INT $ read @Int (c : rest))
    False -> makeLexerError

stringToken :: Stream Token
stringToken = do
  c <- lift . gets $ curChar
  case c == '"' of
    True -> do
      str <- peekWhile (\p -> (p /= '"') && (p /= '\n') && (p /= '\NUL'))
      lift . modify $ moveInput
      e <- lift . gets $ curChar
      if e == '"' then return (STRING str) else makeLexerError
    False -> makeLexerError

identOrKeywordToken :: Stream Token
identOrKeywordToken = do
  c <- lift . gets $ curChar
  case isAlpha c of
    True -> do
      rest <- peekWhile isAlphaNum
      let identifier = c : rest
      case lookup identifier keywords of
        (Just keyword) -> return keyword
        Nothing        -> return $ ID identifier
    False -> makeLexerError

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

-- check if peekPos has on of chatacters
checkPeekChar :: String -> Stream ()
checkPeekChar pattern = do
  pp <- lift . gets $ peekPos
  l <- lift . gets $ curLine
  case (pp < T.length l) of
    True ->
      if (elem (T.index l pp) pattern)
        then return ()
        else
          (lift . modify $ moveInput) >> do
            c <- lift . gets $ curChar
            l' <- lift . gets $ line
            p <- lift . gets $ curPos
            ExceptT . return . Left . LexerError (l', p) $ "unexpected character: '" <> [c] <> ";"
    False -> return ()

makeLexerError :: Stream Token
makeLexerError = do
  c <- lift . gets $ curChar
  l <- lift . gets $ line
  p <- lift . gets $ curPos
  ExceptT . return . Left . LexerError (l, p) $ "unexpected character: '" <> [c] <> ";"

pushToken :: Token -> Stream ()
pushToken token = lift . modify $ (\s -> s {curToken = peekToken s, peekToken = token})

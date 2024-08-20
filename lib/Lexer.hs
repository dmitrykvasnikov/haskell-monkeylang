module Lexer where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Strict
import           Data.Char                        (isAlphaNum, isDigit,
                                                   isLetter)
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Types.Error
import           Types.Token

data Input a = Input { input           :: Text
                     , curPos, peekPos :: Int
                     , ch              :: Char
                     , curTok, peekTok :: Token
                     }
  deriving (Show)

type Lexer a = StateT (Input Token) (Either Error) a

nextToken :: Lexer Token
nextToken = do
  skipWhiteSpaces
  pt <- gets peekTok
  newTok <- asum [mkLexerError, readSingle, readIdentifier, readNumber, readString]
  readChar
  modify (\i -> i {curTok = pt, peekTok = newTok})
  return newTok

getTokens :: Lexer [Token]
getTokens = do
  token <- gets curTok
  case token of
    EOF     -> return []
    NOTOKEN -> nextToken >> getTokens
    tok     -> nextToken >> getTokens >>= \rest -> return $ tok : rest

-- lexers for different kinds of lexem

-- consume chars with co :: (Char -> Bool) -> Lexer String
peekWhile :: (Char -> Bool) -> Lexer String
peekWhile cond = do
  Input i _ pp _ _ _ <- get
  case (pp < T.length i) of
    True -> do
      let c = T.index i pp
      case (cond c) of
        False -> return []
        True -> do
          readChar
          rest <- peekWhile cond
          return $ c : rest
    False -> return []

skipWhiteSpaces :: Lexer ()
skipWhiteSpaces = do
  c <- gets ch
  case elem c " \t\r" of
    True  -> readChar >> skipWhiteSpaces
    False -> return ()

-- single operator
readSingle :: Lexer Token
readSingle = do
  c <- gets ch
  case elem c "\NUL\n\\=;,(){}+-*/<>!" of
    False -> mkLexerError
    True -> case c of
      '\NUL' -> return EOF
      '='    -> return ASSIGN
      '\n'   -> return SEMICOLON
      ';'    -> return SEMICOLON
      ','    -> return COMMA
      '('    -> return LPAREN
      ')'    -> return RPAREN
      '{'    -> return LBRACE
      '}'    -> return RBRACE
      '+'    -> return PLUS
      '-'    -> return MINUS
      '!'    -> return NOT
      '*'    -> return MULT
      '/'    -> return DIV
      '>'    -> return GRT
      '<'    -> return LST
      _      -> mkLexerError

-- double operator
readDouble :: Lexer Token
readDouble = do
  Input i _ pp c _ _ <- get
  case (pp < T.length i) of
    True -> case (c, T.index i pp) of
      ('=', '=') -> readChar >> return EQL
      ('!', '=') -> readChar >> return NOTEQL
      ('<', '>') -> readChar >> return NOTEQL
      ('<', '=') -> readChar >> return LSTEQL
      ('=', '<') -> readChar >> return LSTEQL
      ('=', '>') -> readChar >> return GRTEQL
      ('>', '=') -> readChar >> return GRTEQL
      _          -> mkLexerError
    False -> mkLexerError

-- read number literal
readNumber :: Lexer Token
readNumber = do
  c <- gets ch
  case isDigit c of
    True -> do
      rest <- peekWhile isDigit
      return $ INT (read @Int (c : rest))
    False -> mkLexerError

-- read string literal
readString :: Lexer Token
readString = do
  c <- gets ch
  case c of
    '\"' -> do
      string <- peekWhile (/= '"')
      readChar -- we have to skip quote
      return $ STRING string
    _ -> mkLexerError

-- read keyword / identifier
readIdentifier :: Lexer Token
readIdentifier = do
  c <- gets ch
  case isLetter c of
    True -> do
      rest <- peekWhile isAlphaNum
      let var = c : rest
      case lookup var keywords of
        Just token -> return token
        Nothing    -> return $ ID var
    False -> mkLexerError

-- typical lexer error
mkLexerError :: Lexer Token
mkLexerError = do
  Input _ cp _ c _ _ <- get
  lift $ Left $ LexerError $ "wrong character at position " <> show (cp + 1) <> ": '" <> [c] <> "'"

-- return keyword or identifier
lookUpIdent :: String -> Token
lookUpIdent ident = case lookup ident keywords of
  Just k  -> k
  Nothing -> ID ident

-- get next char from input
readChar :: StateT (Input Token) (Either Error) ()
readChar = do
  Input i _ pp _ _ _ <- get
  modify (\input -> input {ch = if (T.length i) <= pp then '\NUL' else T.index i pp, curPos = pp, peekPos = pp + 1})
  return ()

-- create input from String
mkInput :: String -> Input Token
mkInput ""  = Input (T.pack "") 0 1 '\NUL' EOF EOF
mkInput str = Input (T.pack $ str <> ";") 0 1 (head str) NOTOKEN NOTOKEN

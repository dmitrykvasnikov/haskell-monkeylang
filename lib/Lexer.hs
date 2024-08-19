module Lexer where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Strict
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Types.Error
import           Types.Error                      (Error (LexerError))
import           Types.Token

data Input a = Input { input           :: Text
                     , curPos, peekPos :: Int
                     , ch              :: Char
                     , curTok, peekTok :: Token
                     }
  deriving (Show)

type Lexer = StateT (Input Token) (Either Error) Token

r :: StateT s m a -> s -> m (s, a)
r = undefined

nextToken :: Lexer
nextToken = do
  Input _ cp _ c _ pt <- get
  let newTok = case c of
        '\NUL' -> EOF
        '='    -> ASSIGN
        '\n'   -> SEMICOLON
        ';'    -> SEMICOLON
        ','    -> COMMA
        '('    -> LPAREN
        ')'    -> RPAREN
        '{'    -> LBRACE
        '}'    -> RBRACE
        '+'    -> PLUS
        '-'    -> MINUS
        '*'    -> MULT
        '/'    -> DIV
        _      -> ILLEGAL
  case newTok of
    ILLEGAL -> lift $ Left $ LexerError $ "wrong character at position " <> show (cp + 1) <> ": '" <> [c] <> "'"
    -- \$ Left $ LexerError $ "unknown char at position " <> show pt <> ": '" <> [c] <> "'"
    _ ->
      readChar
        >> modify (\i -> i {curTok = pt, peekTok = newTok})
        >> return newTok

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
mkInput str = Input (T.pack str) 0 1 (head str) NOTOKEN NOTOKEN

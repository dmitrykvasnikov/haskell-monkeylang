module Lexer where

import qualified Constant                   as CONST
import           Control.Monad.State.Strict
import           Data.Bool                  (otherwise)
import           Data.Char                  (chr, isLetter, isNumber)
import           Data.Maybe                 (fromJust)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           GHC.Base                   (undefined)
import           Token
import           Token                      (Token (ASSIGN))

data Input = Input { input   :: Text
                   , pos     :: Int
                   , readPos :: Int
                   , cur     :: Char
                   }
  deriving (Show)

data Branch = NUM | ID | FAULT | SCT | NUL

-- helper function to add _ and numbres to variable names
isLetter' :: Char -> Bool
isLetter' c = isLetter c || elem c "0123456789_"

-- helper function to define branch to parse: ident, digit or illegal
branch :: Char -> Branch
branch c
  | isLetter c = ID
  | isNumber c = NUM
  | elem c CONST.singleCharTokenPattern = SCT
  | c == '\NUL' = NUL
  | otherwise = FAULT

mkInput :: String -> Input
mkInput str = case null str of
  True  -> Input {input = inp, pos = 0, readPos = 1, cur = chr 0}
  False -> Input {input = inp, pos = 0, readPos = 1, cur = T.index inp 0}
  where
    inp = T.pack str

peek :: State Input ()
peek = do
  (Input i p rp _) <- get
  case (rp >= T.length i) of
    True  -> put $ Input i (p + 1) (rp + 1) (chr 0)
    False -> put $ Input i (p + 1) (rp + 1) (T.index i $ p + 1)

-- readChar reads next char in input
readChar :: State Input Char
readChar = do
  Input i _ rp _ <- get
  if (rp >= T.length i) then return '\NUL' else return $ T.index i rp

ws :: State Input ()
ws = do
  Input _ _ _ c <- get
  case elem c " \n\t\r" of
    True  -> peek >> ws
    False -> return ()

readIdentifier :: State Input String
readIdentifier = do
  Input _ _ _ c <- get
  case isLetter' c of
    False -> return []
    True -> do
      peek
      rest <- readIdentifier
      return $ c : rest

readNumber :: State Input String
readNumber = do
  Input _ _ _ c <- get
  case isNumber c of
    False -> return []
    True -> do
      peek
      rest <- readNumber
      return $ c : rest

nextToken :: State Input Token
nextToken = do
  ws
  Input _ _ _ c <- get
  case c of
    '=' -> do
      c' <- readChar
      if c' == '=' then peek >> peek >> return Token.EQ else peek >> return ASSIGN
    '!' -> do
      c' <- readChar
      if c' == '=' then peek >> peek >> return NOT_EQ else peek >> return BANG
    --     '=' -> peek >> return ASSIGN
    --     ',' -> peek >> return COMMA
    --     '+' -> peek >> return PLUS
    --     '-' -> peek >> return MINUS
    --     '*' -> peek >> return ASTERISK
    --     '/' -> peek >> return SLASH
    --     '!' -> peek >> return BANG
    --     '<' -> peek >> return Token.LT
    --     '>' -> peek >> return Token.GT
    --     ';' -> peek >> return SEMICOLON
    --     '(' -> peek >> return LPAREN
    --     ')' -> peek >> return RPAREN
    --     '{' -> peek >> return LBRACE
    --     '}' -> peek >> return RBRACE
    _ -> case branch c of
      SCT -> peek >> (return $ fromJust . lookup c $ CONST.singleCharToken)
      NUL -> return EOF
      ID -> do
        w <- readIdentifier
        let w' = lookup w CONST.keywords
        case w' of
          Nothing -> return $ IDENT w
          Just k  -> return k
      NUM -> do
        n <- read <$> readNumber
        return $ INT n
      FAULT -> return ILLEGAL

lexer :: State Input [Token]
lexer = do
  tok <- nextToken
  case tok of
    EOF -> return [EOF]
    ILLEGAL -> return [ILLEGAL]
    _ -> do
      rest <- lexer
      return $ tok : rest

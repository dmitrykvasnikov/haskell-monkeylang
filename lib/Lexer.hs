module Lexer where

import qualified Constant                   as CONST
import           Control.Monad.State.Strict
import           Data.Char                  (chr, isLetter, isNumber)
import           Data.Maybe                 (fromJust)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import           Token

data Input = Input { input   :: Text
                   , pos     :: (Int, Int)
                   , readPos :: Int
                   , cur     :: Char
                   }
  deriving (Show)

data Branch = NUM | ID | FAULT | SCT | NUL

-- helper function to add _ and numbres to variable names
isLetter' :: Char -> Bool
isLetter' c = isLetter c || elem c "0123456789_'"

-- helper function to define branch to parse: ident, digit or illegal
branch :: Char -> Branch
branch c
  | isLetter c = ID
  | isNumber c = NUM
  | elem c CONST.singleCharTokenPattern = SCT
  | c == '\NUL' = NUL
  | otherwise = FAULT

mkLInputFs :: String -> Input
mkLInputFs str = case null str of
  True  -> Input {input = inp, pos = (1, 0), readPos = 1, cur = chr 0}
  False -> Input {input = inp, pos = (1, 0), readPos = 1, cur = T.index inp 0}
  where
    inp = T.pack str

mkLInputFf :: FilePath -> IO Input
mkLInputFf filepath = do
  content <- T.readFile filepath
  case T.null content of
    True -> return $ Input {input = content, pos = (1, 0), readPos = 1, cur = chr 0}
    False -> return $ Input {input = content, pos = (1, 0), readPos = 1, cur = T.index content 0}

peek :: State Input ()
peek = do
  (Input i (l, p) rp _) <- get
  case (rp >= T.length i) of
    True  -> put $ Input i (l, p + 1) (rp + 1) (chr 0)
    False -> put $ Input i (l, p + 1) (rp + 1) (T.index i $ p + 1)

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

readWithCond :: (Char -> Bool) -> State Input String
readWithCond cond = do
  Input _ _ _ c <- get
  case cond c of
    False -> return []
    True -> do
      peek
      rest <- readWithCond cond
      return $ c : rest

readIdentifier, readNumber :: State Input String
readIdentifier = readWithCond isLetter'
readNumber = readWithCond isNumber

nextToken :: State Input Token
nextToken = do
  ws
  Input _ (l, p) _ c <- get
  case c of
    '=' -> do
      c' <- readChar
      if c' == '=' then peek >> peek >> return Token.EQ else peek >> return ASSIGN
    '!' -> do
      c' <- readChar
      if c' == '=' then peek >> peek >> return NOT_EQ else peek >> return BANG
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
      FAULT -> return $ ERROR $ "Error at line " <> show l <> ", position " <> show (p + 1)

lexer :: State Input [Token]
lexer = do
  tok <- nextToken
  case tok of
    EOF -> return [EOF]
    ERROR err -> return [ERROR err]
    _ -> do
      rest <- lexer
      return $ tok : rest

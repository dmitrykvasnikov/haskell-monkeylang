module Input where

import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State.Strict
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Types.Error
import           Types.Token

type Line = Int

type Col = Int

data Input = Input { input               :: Text
                   , curPos, peekPos     :: Col
                   , curChar             :: Char
                   , pos                 :: (Line, Col)
                   , curLinePos          :: Col
                   , curToken, peekToken :: Token
                   }
  deriving (Show)

type Stream a = ExceptT Error (StateT Input IO) a

todo :: a
todo = undefined

makeInput :: String -> Input
makeInput str =
  Input
    { input = T.pack str,
      curPos = 0,
      peekPos = 1,
      curChar = if length str > 0 then head str else '\NUL',
      pos = (1, 1),
      curLinePos = 0,
      curToken = NOTOKEN,
      peekToken = NOTOKEN
    }

moveInput :: Input -> Input
moveInput i =
  case (peekPos i) < (T.length $ input i) of
    True -> case curChar i of
      '\n' -> i {curChar = T.index (input i) (peekPos i), curPos = 1 + curPos i, peekPos = 1 + peekPos i, pos = newline (pos i), curLinePos = peekPos i}
      _ -> i {curChar = T.index (input i) (peekPos i), curPos = 1 + curPos i, peekPos = 1 + peekPos i, pos = move (pos i)}
    False -> i {curChar = '\NUL'}
  where
    move (x, y) = (x, y + 1)
    newline (x, _) = (x + 1, 1)

currentLine :: Input -> String
currentLine i =
  let src = input i
      start = T.drop (curLinePos i) src
   in T.unpack . T.takeWhile (/= '\n') $ start

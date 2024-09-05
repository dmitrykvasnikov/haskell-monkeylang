module Input where

import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State.Strict
import qualified Data.Map                         as M
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Types.Ast
import           Types.Error
import           Types.Token

type Line = Int

type Col = Int

data Input a = Input { input               :: Text
                     , curPos, peekPos     :: Col
                     , curChar             :: Char
                     , pos                 :: (Line, Col)
                     , curLinePos          :: Col
                     , curToken, peekToken :: (Token, Col)
                     , program             :: [Statement]
                     , heap                :: M.Map String a
                       -- statementPos keeps position of first and last lines of stratemen
                     , statementPos        :: (Col, Col)
                     , isReturn            :: Bool
                     }
  deriving (Show)

type Stream a b = ExceptT Error (StateT (Input a) IO) b

todo :: a
todo = undefined

makeInput :: String -> Input a
makeInput str =
  Input
    { input = T.pack str,
      curPos = 0,
      peekPos = 1,
      curChar = if length str > 0 then head str else '\NUL',
      pos = (1, 1),
      curLinePos = 0,
      curToken = (NOTOKEN, 0),
      peekToken = (NOTOKEN, 0),
      program = [],
      heap = M.empty,
      statementPos = (0, 0),
      isReturn = False
    }

updateInput :: String -> Input a -> Input a
updateInput str inp =
  inp
    { input = T.pack str,
      curPos = 0,
      peekPos = 1,
      curChar = if length str > 0 then head str else '\NUL',
      pos = (1, 1),
      curLinePos = 0,
      curToken = (NOTOKEN, 0),
      peekToken = (NOTOKEN, 0),
      program = [],
      statementPos = (0, 0),
      isReturn = False
    }

moveInput :: Input a -> Input a
moveInput i =
  case (peekPos i) < (T.length $ input i) of
    True -> case curChar i of
      '\n' -> i {curChar = T.index (input i) (peekPos i), curPos = 1 + curPos i, peekPos = 1 + peekPos i, pos = newline (pos i), curLinePos = peekPos i}
      _ -> i {curChar = T.index (input i) (peekPos i), curPos = 1 + curPos i, peekPos = 1 + peekPos i, pos = move (pos i)}
    False -> i {curChar = '\NUL'}
  where
    move (x, y) = (x, y + 1)
    newline (x, _) = (x + 1, 1)

currentLine :: Input a -> String
currentLine i =
  let src = input i
      start = T.drop (curLinePos i) src
   in T.unpack . T.takeWhile (/= '\n') $ start

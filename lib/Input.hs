module Input where

import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State.Strict
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Types.Error
import           Types.Token

type Line = Int

type Col = Int

data Input = Input { input               :: [Text]
                   , curInput            :: Text
                   , curLine             :: Line
                   , curPos, peekPos     :: Col
                   , curChar             :: Char
                   , curToken, peekToken :: Token
                   }
  deriving (Show)

type Stream a = ExceptT Error (StateT Input IO) a

makeInput :: String -> Input
makeInput str =
  let strs = map (T.pack) . lines $ str
   in Input
        { input = if str == "" then [] else strs,
          curInput = if str == "" then T.pack "" else head strs,
          curLine = 1,
          curPos = 0,
          peekPos = 1,
          curChar = if str == "" then '\NUL' else T.head . head $ strs,
          curToken = if str == "" then EOF else NOTOKEN,
          peekToken = if str == "" then EOF else NOTOKEN
        }

moveInput :: Input -> Input
moveInput i =
  case (peekPos i) < (T.length $ curInput i) of
    True -> i {curPos = peekPos i, peekPos = 1 + peekPos i, curChar = T.index (curInput i) (peekPos i)}
    False -> case curLine i >= length (input i) of
      True -> i {curChar = '\NUL'}
      False ->
        let newCur = (input i) !! (curLine i)
         in i {curInput = newCur, curPos = 0, peekPos = 1, curChar = T.head newCur, curLine = 1 + curLine i}

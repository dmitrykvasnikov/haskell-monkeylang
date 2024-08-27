module Input where

import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State.Strict
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Debug.Trace
import           Types.Error
import           Types.Token

type Line = Int

type Col = Int

data Input = Input { input               :: [Text]
                   , curLine             :: Text
                   , line                :: Line
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
          curLine = if str == "" then T.pack "" else head strs,
          line = 1,
          curPos = 0,
          peekPos = 1,
          curChar = if str == "" then '\NUL' else T.head . head $ strs,
          curToken = if str == "" then EOF else NOTOKEN,
          peekToken = if str == "" then EOF else NOTOKEN
        }

moveInput :: Input -> Input
moveInput i =
  case (peekPos i) < (T.length $ curLine i) of
    True -> i {curPos = peekPos i, peekPos = 1 + peekPos i, curChar = T.index (curLine i) (peekPos i)}
    False -> case line i >= length (input i) of
      True -> traceShow ("inpit here" <> show (curPos i)) $ i {curChar = '\NUL'}
      False ->
        let newCur = (input i) !! (line i)
         in i {curLine = newCur, curPos = 0, peekPos = 1, curChar = T.head newCur, line = 1 + line i}

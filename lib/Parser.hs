module Parser where

import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.Except       (runExceptT, throwE)
import           Control.Monad.Trans.State.Strict (gets, runStateT)
import qualified Data.Text                        as T (unpack)
import           Debug.Trace
import           Input
import           Lexer                            (nextToken)
import           Types.Ast
import           Types.Error
import           Types.Token

-- Lowest / Equals / LessOrGreat / Sum / Mult / Prefix / Call / Index
data Precedence = L | E | LG | S | M | P | C | I deriving (Eq, Ord, Show)

type Parser a = Stream a

precedences :: [(Token, Precedence)]
precedences =
  [ (PLUS, S),
    (CONCAT, S),
    (MINUS, S),
    (MULT, M),
    (DIV, M),
    (EQL, E),
    (NOTEQL, E),
    (GRT, LG),
    (LST, LG),
    (GRTEQL, LG),
    (LSTEQL, LG),
    (LPAREN, C),
    (LBRACKET, I)
  ]

runParser :: Parser a -> String -> IO ((Either Error a), Input)
runParser l i = (runStateT . runExceptT) l $ makeInput i

-- parseProgram takes end token, because it's used both for whole program and block statements evaluation
-- parsers for Tokens

-- parseListExpression takes end Token, separator Token and parser

-- helpers for prefix / infix operations
-- getPrefixOp :: Token -> Parser (Parser Expression)
-- getPrefixOp = \case
--   (INT _) -> return parseIntExpression
--   (STRING _) -> return parseStringExpression
--   (ID _) -> return parseIdExpression
--   TRUE -> return parseBoolExpression
--   FALSE -> return parseBoolExpression
--   NOT -> return parsePrefixExpression
--   MINUS -> return parsePrefixExpression
--   RETURN -> return parseReturnExpression
--   LET -> return parseLetExpression
--   IF -> return parseIfExpression
--   token -> return $ makeParseError $ "no prefix operation found for token '" <> show token <> "'"

-- getInfixOp :: Token -> Maybe (Expression -> Parser Expression)
-- getInfixOp = \case
--   PLUS   -> Just parseInfixExpression
--   MINUS  -> Just parseInfixExpression
--   MULT   -> Just parseInfixExpression
--   DIV    -> Just parseInfixExpression
--   GRT    -> Just parseInfixExpression
--   LST    -> Just parseInfixExpression
--   GRTEQL -> Just parseInfixExpression
--   LSTEQL -> Just parseInfixExpression
--   EQL    -> Just parseInfixExpression
--   NOTEQL -> Just parseInfixExpression
--   CONCAT -> Just parseInfixExpression
--   _      -> Nothing

-- helpers
-- check for peekTocken and move it to current position if it's equal to argument

getCurrentToken, getPeekToken :: Parser Token
getCurrentToken = lift . gets $ curToken
getPeekToken = lift . gets $ peekToken

getPrecedence :: Token -> Precedence
getPrecedence token = maybe L id (lookup token precedences)

-- movePeekToken :: [Token] -> Parser Statement
-- movePeekToken tokens = do
--   pToken <- getPeekToken
--   case elem pToken tokens of
--     True -> nextToken
--     False -> nextToken >> (makeParseError $ "unexpected token: got '" <> show pToken <> "', expected '" <> foldl1 (\s t -> s <> "', '" <> t) (map show tokens) <> "'")

makeParseError :: String -> Parser Statement
makeParseError err =
  (lift . gets $ curPos) >>= \c ->
    (lift . gets $ curLine) >>= \l ->
      (lift . gets $ curInput) >>= \src -> throwE . ParserError (l, c) err $ (T.unpack src)

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
parseProgram :: Parser Expression
parseProgram = parseExpressions EOF >>= \exprs -> return $ BlockE exprs

parseExpressions :: Token -> Parser [Expression]
parseExpressions end = do
  token <- lift . gets $ curToken
  let action
        | token == NOTOKEN = nextToken >> parseExpressions end
        | token == end = return []
        | token == SEMICOLON = nextToken >> parseExpressions end
        | token == EOF =
            (lift . gets $ curPos) >>= \c ->
              (lift . gets $ curLine) >>= \l ->
                (lift . gets $ curInput) >>= \src -> throwE . ParserError (c, l) "unexpected end of stream" $ (T.unpack src)
        | otherwise = parseExpression L >>= \expr -> checkPeekToken [SEMICOLON, end] >> nextToken >> parseExpressions end >>= \exprs -> return $ expr : exprs
  action

parseExpression :: Precedence -> Parser Expression
parseExpression pr = do
  prefix <- getPrefixOp =<< (lift . gets $ curToken)
  expr <- prefix
  return expr

-- parsers for Tokens
parseIntExpression, parseStringExpression, parseBoolExpression, parseIdExpression, parsePrefixExpression, parseReturnExpression, parseLetExpression :: Parser Expression
parseIntExpression = getCurrentToken >>= \(INT n) -> return (IntE n)
parseStringExpression = getCurrentToken >>= \(STRING s) -> return (StringE s)
parseIdExpression = getCurrentToken >>= \(ID i) -> return (IdE i)
parseBoolExpression =
  getCurrentToken >>= \case
    TRUE  -> return (BoolE True)
    FALSE -> return (BoolE False)
parsePrefixExpression = do
  op <- getCurrentToken
  nextToken
  expr <- parseExpression L
  return $ UnOp op expr
parseReturnExpression = nextToken >> parseExpression L >>= \expr -> return $ ReturnE expr
parseLetExpression = nextToken >> parseExpression L >>= \right -> movePeekToken ASSIGN >> nextToken >> parseExpression L >>= \left -> return $ LetE left right

getCurrentToken :: Parser Token
getCurrentToken = lift . gets $ curToken

-- helpers for prefix / infix operations
getPrefixOp :: Token -> Parser (Parser Expression)
getPrefixOp = \case
  (INT _) -> return parseIntExpression
  (STRING _) -> return parseStringExpression
  (ID _) -> return parseIdExpression
  TRUE -> return parseBoolExpression
  FALSE -> return parseBoolExpression
  NOT -> return parsePrefixExpression
  MINUS -> return parsePrefixExpression
  RETURN -> return parseReturnExpression
  LET -> return parseLetExpression
  token -> return $ makeParseError $ "no prefix operation found for token '" <> show token <> "'"

-- check for peekToken and move it to current position if it's equal to argument
movePeekToken :: Token -> Parser Expression
movePeekToken token = do
  pToken <- lift . gets $ peekToken
  case pToken == token of
    True -> nextToken >> return NullExpression
    False -> nextToken >> (makeParseError $ "unexpected token: got '" <> show pToken <> "', expected '" <> show token <> "'")

checkPeekToken :: [Token] -> Parser Expression
checkPeekToken tokens = do
  pToken <- lift . gets $ peekToken
  case elem pToken tokens of
    True -> return NullExpression
    False -> nextToken >> (makeParseError $ "unexpected token: got '" <> show pToken <> "', expected '" <> foldl1 (\s t -> s <> "', '" <> t) (map show tokens) <> "'")

makeParseError :: String -> Parser Expression
makeParseError err =
  (lift . gets $ curPos) >>= \c ->
    (lift . gets $ curLine) >>= \l ->
      (lift . gets $ curInput) >>= \src -> throwE . ParserError (l, c) err $ (T.unpack src)

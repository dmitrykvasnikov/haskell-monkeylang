module Parser where

import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.Except       (runExceptT, throwE)
import           Control.Monad.Trans.State.Strict (gets, runStateT)
import qualified Data.Text                        as T
import           Input
import           Lexer                            (nextToken)
import           Types.Ast
import           Types.Error
import           Types.Token

type Parser a = Stream a

-- Lowest / Equals / LessOrGreat / Sum / Mult / Prefix / Call / Index
data Precedence = L | E | LG | S | M | P | C | I deriving (Eq, Ord, Show)

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
parseProgram :: Parser Statement
parseProgram = gP >>= \p1 -> parseStatements EOF >>= \sts -> gP >>= \p2 -> return . BlockS p1 p2 $ sts

parseStatements :: Token -> Parser [Statement]
parseStatements end = do
  token <- getCurrentToken
  let action
        | token == end = return []
        | token == EOF = (lift . gets $ pos) >>= \p -> (lift . gets $ currentLine) >>= \src -> throwE . ParserError p "unexpected end of input stream" $ src
        | token == NOTOKEN = nextToken >> parseStatements end
        | token == SEMICOLON = nextToken >> parseStatements end
        | otherwise = parseStatement >>= \st -> movePeekToken [SEMICOLON, end] >> parseStatements end >>= \sts -> return $ st : sts
  action

parseStatement :: Parser Statement
parseStatement = do
  token <- getCurrentToken
  case token of
    LET    -> parseLetS
    RETURN -> parseReturnS
    _      -> parseExprS

parseExprS, parseLetS, parseReturnS, parseBlockS :: Parser Statement
parseExprS = gP >>= \p1 -> parseExpresion L >>= \expr -> gP >>= \p2 -> return $ ExprS p1 p2 expr
parseLetS = gP >>= \p1 -> nextToken >> parseIdE >>= \var -> movePeekToken [ASSIGN] >> nextToken >> parseExpresion L >>= \expr -> gP >>= \p2 -> return $ LetS p1 p2 var expr
parseReturnS = gP >>= \p1 -> nextToken >> parseExpresion L >>= \expr -> gP >>= \p2 -> return $ ReturnS p1 p2 expr
parseBlockS = gP >>= \p1 -> nextToken >> parseStatements RBRACE >>= \sts -> gP >>= \p2 -> return $ BlockS p1 p2 sts

parseExpresion :: Precedence -> Parser Expr
parseExpresion pr = do
  token <- getCurrentToken
  prefix <- getPrefixOp token
  left <- prefix
  go left
  where
    go :: Expr -> Parser Expr
    go l = do
      op <- lift . gets $ peekToken
      case (op /= SEMICOLON && pr < (getPrecedence op)) of
        True -> do
          case (getInfixOp op) of
            Nothing  -> return l
            (Just f) -> nextToken >> f l >>= go
        False -> return l

parseIntE, parseStringE, parseBoolE, parseIdE, parseGroupedE, parsePrefixE, parseIfE :: Parser Expr
parseIntE = checkCurrentToken [(INT 0)] >>= \(INT n) -> return $ IntE n
parseStringE = checkCurrentToken [(STRING "")] >>= \(STRING s) -> return $ StringE s
parseBoolE = checkCurrentToken [TRUE, FALSE] >>= \b -> return $ BoolE (b == TRUE)
parseIdE = checkCurrentToken [ID "variable name"] >>= \(ID i) -> return $ IdE i
parseGroupedE = nextToken >> parseExpresion L >>= \expr -> movePeekToken [RPAREN] >> return expr
parseIfE = do
  cond <- (movePeekToken [LPAREN] >> nextToken >> parseExpresion L)
  th <- (movePeekToken [RPAREN] >> movePeekToken [THEN] >> movePeekToken [LBRACE] >> parseBlockS)
  pt <- getPeekToken
  if pt == ELSE
    then nextToken >> movePeekToken [LBRACE] >> parseBlockS >>= return . IfE cond th
    else gP >>= \p -> return $ IfE cond th (BlockS p p [])
parsePrefixE = getCurrentToken >>= \op -> nextToken >> (parseExpresion L) >>= return . UnOpE op

parseInfixE :: Expr -> Parser Expr
parseInfixE left = getCurrentToken >>= \op -> nextToken >> parseExpresion (getPrecedence op) >>= return . BinOpE op left

-- parseListExpression takes end Token, separator Token and parser

-- helpers for prefix / infix operations
getPrefixOp :: Token -> Parser (Parser Expr)
getPrefixOp = \case
  (INT _) -> return parseIntE
  (STRING _) -> return parseStringE
  (ID _) -> return parseIdE
  TRUE -> return parseBoolE
  FALSE -> return parseBoolE
  LPAREN -> return parseGroupedE
  NOT -> return parsePrefixE
  MINUS -> return parsePrefixE
  IF -> return parseIfE
  token -> (lift . gets $ pos) >>= \p -> (lift . gets $ currentLine) >>= \src -> throwE . ParserError p ("no prefix operation found for token '" <> show token <> "'") $ src

getInfixOp :: Token -> Maybe (Expr -> Parser Expr)
getInfixOp = \case
  PLUS   -> Just parseInfixE
  MINUS  -> Just parseInfixE
  MULT   -> Just parseInfixE
  DIV    -> Just parseInfixE
  GRT    -> Just parseInfixE
  LST    -> Just parseInfixE
  GRTEQL -> Just parseInfixE
  LSTEQL -> Just parseInfixE
  EQL    -> Just parseInfixE
  NOTEQL -> Just parseInfixE
  CONCAT -> Just parseInfixE
  _      -> Nothing

-- helpers
-- check for peekTocken and move it to current position if it's equal to argument

gP :: Parser Col
gP = lift . gets $ curLinePos

getCurrentToken, getPeekToken :: Parser Token
getCurrentToken = lift . gets $ curToken
getPeekToken = lift . gets $ peekToken

getPrecedence :: Token -> Precedence
getPrecedence token = maybe L id (lookup token precedences)

movePeekToken :: [Token] -> Parser ()
movePeekToken tokens = do
  pToken <- getPeekToken
  case elem pToken tokens of
    True -> nextToken
    False -> nextToken >> (makeParseError_ $ "unexpected token: got '" <> show pToken <> "', expected '" <> foldl1 (\s t -> s <> "'or '" <> t) (map show tokens) <> "'")

checkCurrentToken :: [Token] -> Parser Token
checkCurrentToken tokens = do
  cToken <- getCurrentToken
  case elem cToken tokens of
    True -> return cToken
    False -> (makeParseError $ "unexpected token: got '" <> show cToken <> "', expected '" <> foldl1 (\s t -> s <> "'or '" <> t) (map show tokens) <> "'")

makeParseError :: String -> Parser Token
makeParseError err =
  (lift . gets $ pos) >>= \p ->
    (lift . gets $ currentLine) >>= \src ->
      throwE . ParserError p err $ src

makeParseError_ :: String -> Parser ()
makeParseError_ err =
  (lift . gets $ pos) >>= \p ->
    (lift . gets $ currentLine) >>= \src ->
      throwE . ParserError p err $ src

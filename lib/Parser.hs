module Parser where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Strict
import           Debug.Trace
import           Lexer
import qualified Types.Ast                        as A
import           Types.Error
import qualified Types.Token                      as T

type Parser a = Lexer a

data Precedence = LOWEST | EQUALS | LSGT | SUM | MULT | PREFIX | CALL deriving
  ( Eq
  , Ord
  , Show
  )

precedences :: [(T.Token, Precedence)]
precedences =
  [ (T.PLUS, SUM),
    (T.MINUS, SUM),
    (T.MULT, MULT),
    (T.DIV, MULT),
    (T.EQL, EQUALS),
    (T.NOTEQL, EQUALS),
    (T.GRT, LSGT),
    (T.LST, LSGT),
    (T.GRTEQL, LSGT),
    (T.LSTEQL, LSGT)
  ]

-- parse program into list of statements
parseProgram :: Parser [A.Statement]
parseProgram = do
  ct <- gets curTok
  case ct of
    T.EOF -> return []
    T.SEMICOLON -> nextToken >> parseProgram
    T.NOTOKEN -> nextToken >> parseProgram
    _ -> parseStatement >>= \st -> nextToken >> parseProgram >>= \sts -> return $ st : sts

parseStatement :: Parser A.Statement
parseStatement = do
  ct <- gets curTok
  case ct of
    T.LET    -> parseLetStatement
    T.RETURN -> parseReturnStatement
    _        -> parseExpressionStatement

parseReturnStatement, parseLetStatement, parseExpressionStatement :: Parser A.Statement
parseLetStatement = do
  ident <- getPeekToken (T.ID "variable name") StatementError
  getPeekToken (T.ASSIGN) StatementError
  nextToken
  expr <- parseExpression LOWEST
  getPeekToken (T.SEMICOLON) StatementError
  return $ A.LET ident expr
parseReturnStatement = do
  nextToken
  expr <- parseExpression LOWEST
  getPeekToken (T.SEMICOLON) StatementError
  return $ A.RETURN expr
parseExpressionStatement = do
  expr <- parseExpression (LOWEST)
  getPeekToken (T.SEMICOLON) StatementError
  return $ A.EXPRESSION expr

parseExpression :: Precedence -> Parser A.Expr
parseExpression pr = do
  ct <- gets curTok
  prefix <- getPrefixFn ct
  left <- prefix
  go left
  where
    go :: A.Expr -> Parser A.Expr
    go left' = do
      isSC <- isPeekToken T.SEMICOLON
      prPeek <- peekPrecedence
      case (not isSC) && (pr < prPeek) of
        False -> return left'
        True -> do
          op <- gets peekTok
          infixFn <- getInfixFn op
          case infixFn of
            Nothing -> return left'
            (Just f) -> do
              nextToken
              left'' <- f left'
              go left''

parsePrefixExpression :: Parser A.Expr
parsePrefixExpression =
  gets curTok >>= \op -> nextToken >> parseExpression PREFIX >>= \expr -> return $ A.UNOP op expr

parseInfixExpression :: A.Expr -> Parser A.Expr
parseInfixExpression left =
  gets curTok >>= \op -> curPrecedence >>= \pr -> nextToken >> parseExpression pr >>= \right -> return $ A.BINOP op left right

parseGroupedExpression :: Parser A.Expr
parseGroupedExpression = do
  nextToken
  expr <- parseExpression LOWEST
  getPeekToken T.RPAREN ExpressionError
  return expr

-- parsers for literals
parseIntegralLiteral, parseStringLiteral, parseIdentifier, parseBoolLiteral :: Parser A.Expr
parseIntegralLiteral = gets curTok >>= \(T.INT num) -> return $ A.NUM num
parseStringLiteral = gets curTok >>= \(T.STRING str) -> return $ A.STRING str
parseIdentifier = gets curTok >>= \(T.ID var) -> return $ A.VAR var
parseBoolLiteral = gets curTok >>= \tok -> return $ A.BOOL (tok == T.TRUE)

-- check for current/peek token, expectPeek also moves to next token
isCurToken, isPeekToken, expectPeek :: T.Token -> Parser Bool
isCurToken token = gets curTok >>= \ct -> return $ token == ct
isPeekToken token = gets peekTok >>= \pt -> return $ token == pt
expectPeek token = do
  b <- isPeekToken token
  case b of
    True  -> nextToken >> return True
    False -> return False

-- replace for expectPeek with error message
getPeekToken :: T.Token -> (String -> Error) -> Parser T.Token
getPeekToken token err = do
  pt <- gets peekTok
  case token == pt of
    True  -> nextToken >> return pt
    False -> errorPeekToken token >>= \errMsg -> lift $ Left $ err errMsg

-- error when you don't see expected tooken in peek position
errorPeekToken :: T.Token -> Parser String
errorPeekToken token = do
  pt <- gets peekTok
  return $ "expected next token to be '" <> show token <> "', but got '" <> show pt <> "'"

-- get prefix / infix functions for expression parsing
getPrefixFn :: T.Token -> Parser (Parser A.Expr)
getPrefixFn token = case token of
  (T.INT _) -> return parseIntegralLiteral
  (T.STRING _) -> return parseStringLiteral
  (T.ID _) -> return parseIdentifier
  T.MINUS -> return parsePrefixExpression
  T.NOT -> return parsePrefixExpression
  T.TRUE -> return parseBoolLiteral
  T.FALSE -> return parseBoolLiteral
  T.LPAREN -> return parseGroupedExpression
  _ -> lift $ Left $ ExpressionError $ "do not have prefix operation for '" <> show token <> "'"

getInfixFn :: T.Token -> Parser (Maybe (A.Expr -> Parser A.Expr))
getInfixFn token = case token of
  T.PLUS   -> return $ Just parseInfixExpression
  T.MINUS  -> return $ Just parseInfixExpression
  T.MULT   -> return $ Just parseInfixExpression
  T.DIV    -> return $ Just parseInfixExpression
  T.EQL    -> return $ Just parseInfixExpression
  T.NOTEQL -> return $ Just parseInfixExpression
  T.GRT    -> return $ Just parseInfixExpression
  T.LST    -> return $ Just parseInfixExpression
  T.GRTEQL -> return $ Just parseInfixExpression
  T.LSTEQL -> return $ Just parseInfixExpression
  _        -> return Nothing

-- get precendce of current and peek tokens
peekPrecedence, curPrecedence :: Parser Precedence
peekPrecedence = do
  pt <- gets peekTok
  let precedence = lookup pt precedences
  case precedence of
    Nothing -> return LOWEST
    Just p  -> return p
curPrecedence = do
  ct <- gets curTok
  let precedence = lookup ct precedences
  case precedence of
    Nothing -> return LOWEST
    Just p  -> return p

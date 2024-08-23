module Parser where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Strict
import qualified GHC.Num                          as T
import           Lexer
import qualified Types.Ast                        as A
import qualified Types.Ast                        as E
import           Types.Ast                        (Statement (RETURN))
import           Types.Error
import qualified Types.Token                      as T

type Parser a = Lexer a

data Precedence = LOWEST | EQUALS | LSGT | SUM | MULT | PREFIX | CALL | INDEX deriving
  ( Eq
  , Ord
  , Show
  )

precedences :: [(T.Token, Precedence)]
precedences =
  [ (T.PLUS, SUM),
    (T.CONCAT, SUM),
    (T.MINUS, SUM),
    (T.MULT, MULT),
    (T.DIV, MULT),
    (T.EQL, EQUALS),
    (T.NOTEQL, EQUALS),
    (T.GRT, LSGT),
    (T.LST, LSGT),
    (T.GRTEQL, LSGT),
    (T.LSTEQL, LSGT),
    (T.LPAREN, CALL),
    (T.LBRACKET, INDEX)
  ]

-- parse program into list of statements
parseProgram :: Parser A.Statement
parseProgram = parseStatementsTill T.EOF >>= \sts -> return $ A.BLOCK sts

-- parse list of statemnts till get provided token
parseStatementsTill :: T.Token -> Parser [A.Statement]
parseStatementsTill token = do
  ct <- gets curTok
  if ct == token
    then return []
    else case ct of
      T.NOTOKEN -> nextToken >> parseStatementsTill token
      T.SEMICOLON -> nextToken >> parseStatementsTill token
      T.EOF -> errorPeekToken token >>= \errMsg -> lift $ Left $ StatementError errMsg
      -- _ -> parseStatement >>= \st -> nextToken >> parseStatementsTill token >>= \sts -> return $ st : sts
      _ -> do
        st <- parseStatement
        pt <- gets peekTok
        case elem pt [T.SEMICOLON, T.EOF, token] of
          False -> errorPeekToken T.SEMICOLON >>= \errMsg -> lift $ Left $ StatementError errMsg
          True -> nextToken >> parseStatementsTill token >>= \sts -> return $ st : sts

parseStatement :: Parser A.Statement
parseStatement = do
  ct <- gets curTok
  case ct of
    T.LET    -> parseLetStatement
    T.RETURN -> parseReturnStatement
    _        -> parseExpressionStatement

parseReturnStatement, parseLetStatement, parseExpressionStatement :: Parser A.Statement
parseLetStatement = do
  getPeekToken (T.ID "variable name") StatementError
  ident <- gets curTok
  getPeekToken (T.ASSIGN) StatementError
  nextToken
  expr <- parseExpression LOWEST
  return $ A.LET ident expr
parseReturnStatement = do
  nextToken
  expr <- parseExpression LOWEST
  return $ A.RETURN expr
parseExpressionStatement = do
  expr <- parseExpression (LOWEST)
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
parseGroupedExpression = nextToken >> parseExpression LOWEST >>= \expr -> getPeekToken T.RPAREN ExpressionError >> return expr

parseIfExpression :: Parser A.Expr
parseIfExpression = do
  getPeekToken T.LPAREN ExpressionError
  nextToken
  cond <- parseExpression LOWEST
  getPeekToken T.RPAREN ExpressionError
  getPeekToken T.THEN ExpressionError
  getPeekToken T.LBRACE ExpressionError
  nextToken
  cons <- parseStatementsTill T.RBRACE
  cont <- isPeekToken T.ELSE
  case cont of
    True -> do
      nextToken
      getPeekToken T.LBRACE ExpressionError
      nextToken
      alt <- parseStatementsTill T.RBRACE
      return $ A.IF cond (A.BLOCK cons) (A.BLOCK alt)
    False -> return $ A.IF cond (A.BLOCK cons) (A.BLOCK [])

parseFunctionExpression :: Parser A.Expr
parseFunctionExpression = do
  getPeekToken T.LPAREN ExpressionError
  nextToken
  args <- parseFunctionArguments
  getPeekToken T.LBRACE ExpressionError
  nextToken
  body <- parseStatementsTill T.RBRACE
  return $ A.FN args (A.BLOCK body)

parseFunctionArguments :: Parser [A.Expr]
parseFunctionArguments = do
  c <- gets curTok
  case c of
    T.RPAREN -> return []
    T.ID var -> do
      isComma <- isPeekToken T.COMMA
      case isComma of
        True -> nextToken >> nextToken >> parseFunctionArguments >>= \args -> return $ (A.VAR var) : args
        False -> nextToken >> parseFunctionArguments >>= \args -> return $ (A.VAR var) : args
    _ -> errorCurToken (T.ID "function argument") >>= \errMsg -> lift $ Left $ ExpressionError errMsg

parseCallExpression :: A.Expr -> Parser A.Expr
parseCallExpression func = parseListExpression T.RPAREN >>= \args -> return $ A.CALL func args

parseListExpression :: T.Token -> Parser [A.Expr]
parseListExpression end = do
  b <- isPeekToken end
  case b of
    True -> nextToken >> return []
    False -> do
      nextToken
      args <- go
      getPeekToken end ExpressionError
      return args
  where
    go :: Parser [A.Expr]
    go = do
      arg <- parseExpression LOWEST
      b <- isPeekToken T.COMMA
      case b of
        True  -> nextToken >> nextToken >> go >>= \args -> return $ arg : args
        False -> return [arg]

parseArrayLiteral :: Parser E.Expr
parseArrayLiteral = parseListExpression T.RBRACKET >>= return . A.ARRAY

parseIndexExpression :: A.Expr -> Parser A.Expr
parseIndexExpression arr = nextToken >> parseExpression LOWEST >>= \ind -> getPeekToken T.RBRACKET ExpressionError >> return (A.INDEX ind arr)

-- parsers for literals
parseIntegralLiteral, parseStringLiteral, parseIdentifier, parseBoolLiteral :: Parser A.Expr
parseIntegralLiteral = gets curTok >>= \(T.INT num) -> return $ A.NUM num
parseStringLiteral = gets curTok >>= \(T.STRING str) -> return $ A.STRING str
parseIdentifier = gets curTok >>= \(T.ID var) -> return $ A.VAR var
parseBoolLiteral = gets curTok >>= \tok -> return $ A.BOOL $ show (tok == T.TRUE)

-- check for current/peek token, expectPeek also moves to next token
isCurToken, isPeekToken :: T.Token -> Parser Bool
isCurToken token = gets curTok >>= \ct -> return $ token == ct
isPeekToken token = gets peekTok >>= \pt -> return $ token == pt

-- replace for expectPeek with error message
getPeekToken :: T.Token -> (String -> Error) -> Parser ()
getPeekToken token err = do
  pt <- gets peekTok
  case token == pt of
    True  -> nextToken >> return ()
    False -> errorPeekToken token >>= \errMsg -> lift $ Left $ err errMsg

-- error when you don't see expected tooken in peek position
errorPeekToken, errorCurToken :: T.Token -> Parser String
errorPeekToken token = gets peekTok >>= \pt -> return $ "expected next token to be '" <> show token <> "', but got '" <> show pt <> "'"
errorCurToken token = gets curTok >>= \ct -> return $ "expected next token to be '" <> show token <> "', but got '" <> show ct <> "'"

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
  T.LBRACKET -> return parseArrayLiteral
  T.IF -> return parseIfExpression
  T.FUNCTION -> return parseFunctionExpression
  _ -> lift $ Left $ ExpressionError $ "do not have prefix operation for '" <> show token <> "'"

getInfixFn :: T.Token -> Parser (Maybe (A.Expr -> Parser A.Expr))
getInfixFn token = case token of
  T.CONCAT   -> return $ Just parseInfixExpression
  T.PLUS     -> return $ Just parseInfixExpression
  T.MINUS    -> return $ Just parseInfixExpression
  T.MULT     -> return $ Just parseInfixExpression
  T.DIV      -> return $ Just parseInfixExpression
  T.EQL      -> return $ Just parseInfixExpression
  T.NOTEQL   -> return $ Just parseInfixExpression
  T.GRT      -> return $ Just parseInfixExpression
  T.LST      -> return $ Just parseInfixExpression
  T.GRTEQL   -> return $ Just parseInfixExpression
  T.LSTEQL   -> return $ Just parseInfixExpression
  T.LPAREN   -> return $ Just parseCallExpression
  T.LBRACKET -> return $ Just parseIndexExpression
  _          -> return Nothing

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

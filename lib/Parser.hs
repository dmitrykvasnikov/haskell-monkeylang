module Parser where

import           AST
import           Control.Monad.State.Strict
import           Debug.Trace
import           Token

data Input = Input { tokens         :: [Token]
                   , cToken, pToken :: Token
                   }
  deriving (Show)

mkPInput :: [Token] -> Input
mkPInput []             = Input [] EOF EOF
mkPInput [t]            = Input [] t EOF
mkPInput (ct : pt : ts) = Input ts ct pt

nextToken :: State Input ()
nextToken = do
  Input ts _ pt <- get
  case ts of
    []         -> put (Input [] pt EOF) >> return ()
    (t : rest) -> put (Input rest pt t) >> return ()

expectPeek :: Token -> State Input Bool
expectPeek tok = do
  Input _ _ pt <- get
  case tok == pt of
    True  -> nextToken >> return True
    False -> return False

parseStatement :: State Input Statement
parseStatement = do
  skips SEMICOLON
  Input _ ct _ <- get
  case ct of
    Token.RETURN -> parseReturnStatement
    Token.LET    -> parseLetStatement
    Token.EOF    -> return $ AST.ERROR " there is no token to parse"
    _            -> parseExprStatement

parseStatements :: State Input [Statement]
parseStatements = do
  skips SEMICOLON
  Input _ ct _ <- get
  case ct of
    EOF -> return []
    _ -> do
      st <- parseStatement
      case st of
        (AST.ERROR err) -> return [AST.ERROR err]
        _ -> do
          nextToken
          sts <- parseStatements
          return $ st : sts

-- TODO - errors in Expressions
parseIdentifier :: State Input Expr
parseIdentifier = do
  Input _ cur _ <- get
  case cur of
    (IDENT name) -> return $ Var name
    _ -> return $ Error $ "indentifier expected, but got " <> show cur

parseIntLiteral :: State Input Expr
parseIntLiteral = do
  Input _ cur _ <- get
  case cur of
    (INT n) -> return $ NumLit n
    _       -> error "Never gonna happen"

parseBoolLiteral :: State Input Expr
parseBoolLiteral = do
  Input _ cur _ <- get
  case cur of
    (Token.TRUE)  -> return $ BoolLit True
    (Token.FALSE) -> return $ BoolLit False
    _             -> error "Never gonna happen"

parseExprStatement :: State Input Statement
parseExprStatement = do
  expr <- parseExpression LOW
  case expr of
    (Error err) -> return $ AST.ERROR err
    _           -> return $ EXPR expr

parseExpression :: Precedence -> State Input Expr
parseExpression prec = do
  Input _ ct _ <- get
  case getPrefixParserFns ct of
    Just prefix -> do
      left <- prefix
      recurse prec left
    Nothing -> return $ AST.Error $ "no prefix function for " <> show ct
  where
    recurse :: Precedence -> Expr -> State Input Expr
    recurse prec' left = do
      Input _ _ pt <- get
      peekPrec <- peekPrecedence
      if (pt /= Token.SEMICOLON) && prec' < peekPrec
        then case getInfixParserFns pt of
          Nothing -> return left
          Just inf -> do
            nextToken
            left' <- inf left
            recurse prec' left'
        else return left

parsePrefixExpression :: State Input Expr
parsePrefixExpression = do
  Input _ ct _ <- get
  nextToken
  expr <- parseExpression (PREFIX)
  return $ UnOp (go ct) expr
  where
    go Token.MINUS = NEG
    go BANG        = NOT
    go _           = error "Never gonna happen"

parseInfixExpression :: Expr -> State Input Expr
parseInfixExpression left = do
  Input _ op _ <- get
  prec <- curPrecedence
  nextToken
  right <- parseExpression prec
  return $ BinOp (go op) left right
  where
    go Token.EQ     = AST.EQ
    go Token.NOT_EQ = AST.NOT_EQ
    go Token.LT     = AST.LT
    go Token.GT     = AST.GT
    go Token.PLUS   = AST.PLUS
    go Token.MINUS  = AST.MINUS
    go Token.DIV    = AST.DIV
    go Token.MULT   = AST.MULT
    go _            = error "Never gonna happen"

parseGropupedExpression :: State Input Expr
parseGropupedExpression = do
  nextToken
  expr <- parseExpression LOW
  cond <- expectPeek RPAREN
  case cond of
    True  -> return expr
    False -> return $ Error $ "Right parenthes expected"

--   Input _ _ pt <- get
--   case pt of
--     Token.RPAREN -> nextToken >> return expr
--     _ -> return $ AST.Error $ "Right parenthes expected, but got " <> show pt

parseReturnStatement :: State Input Statement
parseReturnStatement = do
  nextToken
  expr <- parseExpression LOW
  case expr of
    AST.Error err -> return $ AST.ERROR err
    _             -> return $ AST.RETURN expr

parseLetStatement :: State Input Statement
parseLetStatement = do
  Input _ _ pt <- get
  cond1 <- expectPeek (IDENT "ident")
  if not cond1
    then return $ AST.ERROR $ "identifier inspected in LET statement, but got " <> show pt
    else do
      Input _ _ pt <- get
      var <- parseIdentifier
      cond2 <- expectPeek (ASSIGN)
      if not cond2
        then return $ AST.ERROR $ "equal signs is expected after identifier, but got " <> show pt
        else do
          nextToken
          expr <- parseExpression LOW
          case expr of
            (Error err) -> return $ AST.ERROR err
            _           -> return $ AST.LET var expr

-- helper functions for Expression
getPrefixParserFns :: Token -> Maybe (State Input Expr)
getPrefixParserFns tok =
  case tok of
    (Token.IDENT _) -> Just parseIdentifier
    (Token.INT _)   -> Just parseIntLiteral
    Token.TRUE      -> Just parseBoolLiteral
    Token.FALSE     -> Just parseBoolLiteral
    Token.BANG      -> Just parsePrefixExpression
    Token.MINUS     -> Just parsePrefixExpression
    Token.LPAREN    -> Just parseGropupedExpression
    _               -> Nothing

getInfixParserFns :: Token -> Maybe (Expr -> State Input Expr)
getInfixParserFns tok =
  case tok of
    Token.EQ     -> Just parseInfixExpression
    Token.NOT_EQ -> Just parseInfixExpression
    Token.PLUS   -> Just parseInfixExpression
    Token.MINUS  -> Just parseInfixExpression
    Token.GT     -> Just parseInfixExpression
    Token.LT     -> Just parseInfixExpression
    Token.DIV    -> Just parseInfixExpression
    Token.MULT   -> Just parseInfixExpression
    _            -> Nothing

peekPrecedence :: State Input Precedence
peekPrecedence = do
  Input _ _ pt <- get
  case lookup pt precedences of
    Just p  -> return p
    Nothing -> return LOW

curPrecedence :: State Input Precedence
curPrecedence = do
  Input _ ct _ <- get
  case lookup ct precedences of
    Just p  -> return p
    Nothing -> return LOW

skip :: Token -> State Input ()
skip tok = do
  Input _ ct _ <- get
  if tok == ct then nextToken >> return () else return ()

skips :: Token -> State Input ()
skips tok = do
  Input _ ct _ <- get
  if ct == EOF || ct /= tok then return () else nextToken >> skips tok

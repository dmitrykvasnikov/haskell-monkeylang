module Parser where

import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.Except       (runExceptT, throwE)
import           Control.Monad.Trans.State.Strict (gets, modify, runStateT)
import qualified Data.Map                         as M
import           Input
import           Lexer                            (nextToken)
import           Types.Ast
import           Types.Error
import           Types.Token

type Parser a = Stream a

-- Lowest / Equals / LessOrGreat / Sum / Mult / Prefix / Call / Index
data Precedence = L | E | O | LG | S | M | P | C | I deriving (Eq, Ord, Show)

precedences :: [(Token, Precedence)]
precedences =
  [ (PLUS, S),
    (CONCAT, S),
    (MINUS, S),
    (MULT, M),
    (DIV, M),
    (EQL, E),
    (NOTEQL, E),
    (OR, O),
    (AND, O),
    (GRT, LG),
    (LST, LG),
    (GRTEQL, LG),
    (LSTEQL, LG),
    (LPAREN, C),
    (LBRACKET, I)
  ]

runParser :: Parser a -> String -> IO ((Either Error a), Input)
runParser l i = (runStateT . runExceptT) l $ makeInput i

parseProgram :: Parser ()
parseProgram = do
  token <- getCurrentToken
  let action
        | token == EOF = return ()
        | elem token [NOTOKEN, SEMICOLON, SEMICOLON_] = nextToken >> parseProgram
        | otherwise = parseStatement >>= pushStatement >> movePeekToken [SEMICOLON, SEMICOLON_, EOF] >> parseProgram
  action

parseStatement :: Parser Statement
parseStatement = do
  token <- getCurrentToken
  case token of
    LET    -> parseLetS
    RETURN -> parseReturnS
    _      -> parseExprS

parseExprS, parseLetS, parseReturnS, parseBlockS :: Parser Statement
parseExprS = getCurLine >>= \p1 -> parseExpresion L >>= \expr -> getCurLine >>= \p2 -> return $ ExprS p1 p2 expr
parseLetS = getCurLine >>= \p1 -> nextToken >> parseIdE >>= \var -> movePeekToken [ASSIGN] >> nextToken >> parseExpresion L >>= \expr -> getCurLine >>= \p2 -> return $ LetS p1 p2 var expr
parseReturnS = getCurLine >>= \p1 -> nextToken >> parseExpresion L >>= \expr -> getCurLine >>= \p2 -> return $ ReturnS p1 p2 expr
parseBlockS = do
  p1 <- getCurLine
  pt <- getPeekToken
  if pt == RBRACE
    then nextToken >> (return $ BlockS p1 p1 [])
    else do nextToken >> parseStatement >>= \st -> nextToken >> go >>= \sts -> getCurLine >>= \p2 -> return $ BlockS p1 p2 (st : sts)
  where
    go :: Parser [Statement]
    go = do
      pt' <- getCurrentToken
      case pt' of
        SEMICOLON -> nextToken >> go
        SEMICOLON_ -> nextToken >> go
        RBRACE -> return []
        _ -> parseStatement >>= \st' -> nextToken >> go >>= \sts' -> return $ st' : sts'

parseExpresion :: Precedence -> Parser Expr
parseExpresion pr = do
  token <- getCurrentToken
  prefix <- getPrefixOp token
  left <- prefix
  go left
  where
    go :: Expr -> Parser Expr
    go l = do
      op <- getPeekToken
      case (op /= SEMICOLON && op /= SEMICOLON_ && pr < (getPrecedence op)) of
        True -> do
          case (getInfixOp op) of
            Nothing  -> return l
            (Just f) -> nextToken >> f l >>= go
        False -> return l

parseIntE, parseStringE, parseBoolE, parseIdE, parseGroupedE, parsePrefixE, parseIfE, parseFunctionE, parseArrayE, parseHashE :: Parser Expr
parseIntE = checkCurrentToken [(INT 0)] >>= \(INT n) -> return $ IntE n
parseStringE = checkCurrentToken [(STRING "")] >>= \(STRING s) -> return $ StringE s
parseBoolE = checkCurrentToken [TRUE, FALSE] >>= \b -> return $ BoolE (b == TRUE)
parseIdE = checkCurrentToken [ID "variable name"] >>= \(ID i) -> return $ IdE i
parseGroupedE = nextToken >> parseExpresion L >>= \expr -> movePeekToken [RPAREN] >> return expr
parseIfE = do
  cond <- (movePeekToken [LPAREN] >> nextToken >> parseExpresion L)
  th <- (movePeekToken [RPAREN] >> sps >> movePeekToken [THEN] >> sps >> movePeekToken [LBRACE] >> sps >> parseBlockS)
  pt <- (sps >> getPeekToken)
  if pt == ELSE
    then nextToken >> sps >> movePeekToken [LBRACE] >> sps >> parseBlockS >>= return . IfE cond th
    else getCurLine >>= \p -> return $ IfE cond th (BlockS p p [])
parseFunctionE = movePeekToken [LPAREN] >> parseListE RPAREN COMMA parseIdE >>= \args -> movePeekToken [LBRACE] >> sps >> parseBlockS >>= return . FnE args
parseArrayE = parseListE RBRACKET COMMA (parseExpresion L) >>= return . ArrayE
parseHashE = parseListE RBRACE COMMA parseKeyValue >>= return . HashE . M.fromList . map (\(PairE k v) -> (k, v))
parsePrefixE = getCurrentToken >>= \op -> nextToken >> (parseExpresion L) >>= return . UnOpE op

parseInfixE, parseCallE, parseIndexE :: Expr -> Parser Expr
parseInfixE left = getCurrentToken >>= \op -> nextToken >> parseExpresion (getPrecedence op) >>= return . BinOpE op left
parseCallE f = parseListE RPAREN COMMA (parseExpresion L) >>= return . CallE f
parseIndexE e = nextToken >> parseExpresion L >>= \i -> movePeekToken [RBRACKET] >> (return $ IndexE e i)

parseKeyValue :: Parser Expr
parseKeyValue = parseExpresion L >>= \k -> movePeekToken [COLON] >> nextToken >> parseExpresion L >>= return . PairE k

-- parseListE takes end Token, separator Token and parser
parseListE :: Token -> Token -> Parser Expr -> Parser [Expr]
parseListE end sep parser = do
  pt <- getPeekToken
  case pt == end of
    True -> nextToken >> return []
    False -> nextToken >> parser >>= \token -> go >>= \rest -> return $ token : rest
  where
    go :: Parser [Expr]
    go = do
      pt <- getPeekToken
      case pt == end of
        True -> nextToken >> return []
        False -> movePeekToken [sep] >> nextToken >> parser >>= \t -> go >>= \ts -> return $ t : ts

-- helpers for prefix / infix operations
getPrefixOp :: Token -> Parser (Parser Expr)
getPrefixOp = \case
  (INT _) -> return parseIntE
  (STRING _) -> return parseStringE
  (ID _) -> return parseIdE
  TRUE -> return parseBoolE
  FALSE -> return parseBoolE
  LPAREN -> return parseGroupedE
  LBRACKET -> return parseArrayE
  LBRACE -> return parseHashE
  NOT -> return parsePrefixE
  MINUS -> return parsePrefixE
  IF -> return parseIfE
  FUNCTION -> return parseFunctionE
  token -> (lift . gets $ pos) >>= \p -> (lift . gets $ currentLine) >>= \src -> throwE . ParserError p ("no prefix operation found for token '" <> show token <> "'") $ src

getInfixOp :: Token -> Maybe (Expr -> Parser Expr)
getInfixOp = \case
  PLUS     -> Just parseInfixE
  MINUS    -> Just parseInfixE
  MULT     -> Just parseInfixE
  DIV      -> Just parseInfixE
  GRT      -> Just parseInfixE
  LST      -> Just parseInfixE
  GRTEQL   -> Just parseInfixE
  LSTEQL   -> Just parseInfixE
  EQL      -> Just parseInfixE
  OR       -> Just parseInfixE
  AND      -> Just parseInfixE
  NOTEQL   -> Just parseInfixE
  CONCAT   -> Just parseInfixE
  LPAREN   -> Just parseCallE
  LBRACKET -> Just parseIndexE
  _        -> Nothing

-- helpers
-- check for peekTocken and move it to current position if it's equal to argument

pushStatement :: Statement -> Parser ()
pushStatement st = lift . modify $ (\e -> e {program = (program e) ++ [st]})

getCurrentToken, getPeekToken :: Parser Token
getCurrentToken = fst <$> (lift . gets $ curToken)
getPeekToken = fst <$> (lift . gets $ peekToken)

getCurLine :: Parser Int
getCurLine = snd <$> (lift . gets $ curToken)

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

skipPeekToken :: Token -> Parser ()
skipPeekToken token = do
  pt <- getPeekToken
  case pt == token of
    True  -> nextToken >> skipPeekToken token
    False -> return ()

sps :: Parser ()
sps = skipPeekToken SEMICOLON_

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

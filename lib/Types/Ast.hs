module Types.Ast where

import qualified Types.Token as T

-- Elemenst of AST for parser
data Statement = EXPRESSION Expr
               | LET T.Token Expr
               | RETURN Expr
               -- | STs [Statement]
               | NUL

instance Show Statement where
  show (EXPRESSION expr)      = show expr
  show (LET (T.ID name) expr) = "let " ++ name ++ " = " ++ show expr
  show (RETURN expr)          = "return " ++ show expr
  show NUL                    = "There is no valid statement"
  show _                      = "There is no show instance for statement"

data Expr = VAR String
          | NUM Int
          | BOOL Bool
          | STRING String
          | UNOP T.Token Expr
          | BINOP T.Token Expr Expr

-- \| IFEx Expr Expr Expr

instance Show Expr where
  show (VAR name) = name
  show (UNOP tok expr) = "(" ++ (if tok == T.MINUS then "-" else "not ") ++ show expr ++ ")"
  show (BINOP op expr1 expr2) = "(" ++ show expr1 ++ " " ++ show op ++ " " ++ show expr2 ++ ")"
  show (NUM n) = show n
  show (STRING s) = show s
  show (BOOL b) = show b

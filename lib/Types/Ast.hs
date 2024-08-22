module Types.Ast where

import           Data.List   (intercalate)
import qualified Types.Token as T

data Statement = EXPRESSION Expr
               | LET T.Token Expr
               | RETURN Expr
               | BLOCK [Statement]
               | NUL
  deriving (Eq)

instance Show Statement where
  show (EXPRESSION expr)      = show expr
  show (LET (T.ID name) expr) = "let " ++ name ++ " = " ++ show expr
  show (RETURN expr)          = "return " ++ show expr
  show (BLOCK sts)            = "{" <> intercalate ";\n" (map show sts) <> "}"
  show NUL                    = "There is no valid statement"
  show _                      = "There is no show instance for statement"

data Expr = VAR String
          | NUM String
          | BOOL String
          | STRING String
          | UNOP T.Token Expr
          | BINOP T.Token Expr Expr
          | IF Expr Statement Statement
          | FN [Expr] Statement
          | CALL Expr [Expr]
  deriving (Eq)

instance Show Expr where
  show (VAR name) = name
  show (UNOP tok expr) = "(" ++ (if tok == T.MINUS then "-" else "not ") ++ show expr ++ ")"
  show (BINOP op expr1 expr2) = "(" ++ show expr1 ++ " " ++ show op ++ " " ++ show expr2 ++ ")"
  show (NUM n) = show n
  show (STRING s) = show s
  show (BOOL b) = show b
  show (IF cond cons alt) = "if " <> show cond <> " then " <> show cons <> "\n  else " <> show alt
  show (FN ids body) = "function(" <> intercalate ", " (map show ids) <> ")\n" <> show body
  show (CALL fn args) = show fn <> "(" <> intercalate ", " (map show args) <> ")"

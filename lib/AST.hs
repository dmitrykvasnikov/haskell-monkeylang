module AST where

import qualified Token

type Name = String

data Precedence = LOW | EQLS | LTGT | SUM | PROD | PREFIX | CALL deriving
  ( Eq
  , Ord
  )

data Op = NOT | NEG | PLUS | MINUS | MULT | DIV | EQ | NOT_EQ | GT | LT

data Expr = Var Name
          | UnOp Op Expr
          | BinOp Op Expr Expr
          | NumLit Int
          | BoolLit Bool
          | Error String

data Statement = EXPR Expr
               | LET Expr Expr
               | RETURN Expr
               | ERROR String
               | NUL

instance Show Op where
  show NOT    = "not"
  show NEG    = "-"
  show PLUS   = "+"
  show MINUS  = "-"
  show MULT   = "*"
  show DIV    = "/"
  show AST.GT = ">"
  show AST.LT = "<"
  show AST.EQ = "=="
  show NOT_EQ = "!="

instance Show Expr where
  show (Var name) = name
  show (UnOp op expr) = "(" <> show op <> show expr <> ")"
  show (BinOp op expr1 expr2) = "(" <> show expr1 <> " " <> show op <> " " <> show expr2 <> ")"
  show (NumLit n) = show n
  show (BoolLit b) = show $ b == True
  show (Error err) = "Parsing error: " <> err

instance Show Statement where
  show (EXPR expr)        = show expr
  show (AST.LET var expr) = "let " <> show var <> " = " <> show expr
  show (AST.ERROR err)    = "Parsing error: " <> err
  show (AST.RETURN expr)  = "return " <> show expr
  show NUL                = "End of token input"

precedences :: [(Token.Token, Precedence)]
precedences = [(Token.EQ, EQLS), (Token.NOT_EQ, EQLS), (Token.LT, LTGT), (Token.GT, LTGT), (Token.PLUS, SUM), (Token.MINUS, SUM), (Token.DIV, PROD), (Token.MULT, PROD)]

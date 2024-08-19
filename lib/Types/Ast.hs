module Types.Ast where

import           Types.Token

-- Elemenst of AST for parser
data Statement = EXPRESSIONSt Expr
               | LETSt Token Expr
               | RETURNSt Expr
               -- | STs [Statement]
               | NULSt

instance Show Statement where
  show (EXPRESSIONSt expr)    = show expr
  show (LETSt (ID name) expr) = "let " ++ name ++ " " ++ show expr
  show (RETURNSt expr)        = "return " ++ show expr
  show NULSt                  = "There is no valid statement"
  show _                      = "There is no show instance for statement"

data Expr = VAREx Token
          | UNOPEx Token Expr
          | BINOPEx Token Expr Expr
          -- | IFEx Expr Expr Expr
          | NULEx

instance Show Expr where
  show (VAREx (ID name)) = name
  show (UNOPEx tok expr) = show tok ++ show expr
  show (BINOPEx op expr1 expr2) = "(" ++ show expr1 ++ " " ++ show op ++ " " ++ show expr2 ++ ")"
  show NULEx = "There is no valid expression"
  show _ = "There is no show instance for expression"

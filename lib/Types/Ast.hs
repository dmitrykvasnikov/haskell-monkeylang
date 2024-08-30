module Types.Ast where

import           Types.Token

data Expr = IntE Int
          | BoolE Bool
          | StringE String
          | IdE String

data Statement = ExprS Expr
               | LetS Expr Expr
               | ReturnS Expr

instance Show Expr where
  show (IntE n)    = show n
  show (StringE s) = show s
  show (BoolE b)   = if b then "True" else "False"
  show (IdE i)     = i

instance Show Statement where
  show (ExprS e)   = show e
  show (ReturnS e) = "return " <> show e
  show (LetS i e)  = "let " <> show i <> " = " <> show e

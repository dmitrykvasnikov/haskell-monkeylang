module Types.Ast where

import           Types.Token

data Expression = IntE Int
                | BoolE Bool
                | StringE String
                | IdE String
                | ReturnE Expression
                | LetE Expression Expression
                | BlockE [Expression]
                | UnOp Token Expression
                -- for internal purpose only - to clean code
                | NullExpression

instance Show Expression where
  show (IntE n) = show n
  show (BoolE b) = if b then "True" else "False"
  show (StringE s) = show s
  show (IdE i) = i
  show (ReturnE e) = "return " <> show e
  show (LetE i e) = "let " <> show i <> " = " <> show e
  show (BlockE es) = "{" <> concat (map (\e -> (show e) <> ";\n") es) <> "}"
  show (NullExpression) = "You should not use this constructor in production"
  show (UnOp op e) = show op <> " " <> show e

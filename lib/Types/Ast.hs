module Types.Ast where

import           Data.HashMap.Internal.Array (Array)
import           Data.List                   (intercalate)
import           Data.Map                    (Map)
import qualified Data.Map                    as M
import           Types.Token

type Pos = Int

data Expr = IntE Int
          | BoolE Bool
          | StringE String
          | IdE String
          | BinOpE Token Expr Expr
          | UnOpE Token Expr
          | IfE Expr Statement Statement
          | FnE [Expr] Statement
          | CallE Expr [Expr]
          | ArrayE [Expr]
          | HashE (Map Expr Expr)
          | IndexE Expr Expr
          -- internal for parsing of HashE
          | PairE Expr Expr
  deriving (Eq, Ord)

-- each statement keeps position of first and last lines of expression
data Statement = ExprS Pos Pos Expr
               | LetS Pos Pos Expr Expr
               | ReturnS Pos Pos Expr
               | BlockS Pos Pos [Statement]
  deriving (Eq, Ord)

instance Show Expr where
  show (IntE n) = show n
  show (StringE s) = show s
  show (BoolE b) = if b then "True" else "False"
  show (IdE i) = i
  show (UnOpE t e) = "(" <> show t <> show e <> ")"
  show (BinOpE t e1 e2) = "(" <> show e1 <> " " <> show t <> " " <> show e2 <> ")"
  show (IfE c t e@(BlockS _ _ sts)) = "if " <> show c <> " then\n" <> show t <> (if sts == [] then "" else "\nelse\n" <> show e)
  show (FnE args body) = "function (" <> intercalate ", " (map show args) <> ")\n" <> show body
  show (CallE i args) = show i <> "(" <> intercalate ", " (map show args) <> ")"
  show (ArrayE arr) = "[" <> intercalate ", " (map show arr) <> "]"
  show (IndexE e i) = show e <> "[" <> show i <> "]"
  show (HashE hash) = "{" <> intercalate ", " (map (\(k, v) -> show k <> ":" <> show v) (M.toList hash)) <> "}"

-- show (FnE args) = "function (" <> intercalate ", " (map show args) <> ")\n"

instance Show Statement where
  show (ExprS _ _ e)    = show e
  show (ReturnS _ _ e)  = "return " <> show e
  show (LetS _ _ i e)   = "let " <> show i <> " = " <> show e
  show (BlockS _ _ sts) = "{ " <> intercalate "\n  " (map show sts) <> " }"

module Types.Object where

import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State.Strict
import qualified Data.HashMap.Internal            as H
import           Data.List                        (intercalate)
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as M
import           Types.Ast
import           Types.Error

data ObjectType = INTEGER_OBJ | STRING_OBJ | BOOL_OBJ | FUNCTION_OBJ | ARRAY_OBJ | HASH_MAP | NULL_OBJ | ANY_OBJECT deriving
  ( Ord
  )

instance Show ObjectType where
  show INTEGER_OBJ  = "INT"
  show STRING_OBJ   = "STRING"
  show BOOL_OBJ     = "BOOl"
  show FUNCTION_OBJ = "FUNCTION"
  show ARRAY_OBJ    = "ARRAY"
  show NULL_OBJ     = "NULL"
  show HASH_MAP     = "HASH_MAP"
  show _            = "no show instance"

instance Eq ObjectType where
  ANY_OBJECT == _ = True
  _ == ANY_OBJECT = True
  o1 == o2        = show o1 == show o2

data Value = IntV Int
           | StringV String
           | BoolV Bool
           | FnV [String] Statement (M.Map String Object)
           | ArrayV [Object]
           | HashV (M.Map H.Hash (Object, Object))
           | NullV
  deriving (Eq, Ord)

instance Show Value where
  show (IntV i) = show i
  show (StringV s) = show s
  show (BoolV b) = if b then "True" else "False"
  show (NullV) = "null"
  show (FnV args body _) = "function(" <> intercalate (", ") args <> ")" <> show body
  show (ArrayV arr) = "[" <> intercalate (", ") (map show arr) <> "]"
  show (HashV hs) = "{" <> intercalate ", " (map (\(_, (k, v)) -> show k <> " : " <> show v) . M.toList $ hs) <> "}"

data Object = Object { oType :: ObjectType
                     , value :: Value
                     }
  deriving (Eq, Ord)

instance Show Object where
  show (Object t v) = show t <> " :: " <> show v

-- constants for Eval
trueConst, falseConst, nullConst :: Object
trueConst = Object BOOL_OBJ (BoolV True)
falseConst = Object BOOL_OBJ (BoolV False)
nullConst = Object NULL_OBJ NullV

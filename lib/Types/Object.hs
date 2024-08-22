module Types.Object where

import           Data.List       (intercalate)
import           Data.Map.Strict (Map)
import           Types.Ast       as A

data ObjectType = INTEGER_OBJ | BOOLEAN_OBJ | NULL_OBJ | STRING_OBJ | ANY_OBJ

instance Show ObjectType where
  show INTEGER_OBJ = "INT"
  show BOOLEAN_OBJ = "BOOl"
  show STRING_OBJ  = "STRING"
  show _           = "OBJECT"

instance Eq ObjectType where
  ANY_OBJ == _ = True
  _ == ANY_OBJ = True
  obj1 == obj2 = show obj1 == show obj2

data Object = Object { oType :: ObjectType
                     , value :: String
                     }
            | Function { args :: [A.Expr]
                       , body :: A.Statement
                       , env  :: Map String Object
                       }
            | Builtin { inp  :: [ObjectType]
                      , out  :: ObjectType
                      , func :: [Object] -> Object
                      }

instance Eq Object where
  (Object t1 v1) == (Object t2 v2) = t1 == t2 && v1 == v2
  _ == _                           = False

nullCONST, trueCONST, falseCONST :: Object
nullCONST = Object NULL_OBJ "null"
trueCONST = Object BOOLEAN_OBJ "true"
falseCONST = Object BOOLEAN_OBJ "false"

instance Show Object where
  -- show (Object o v) = "Type " <> show o <> " | Value " <> v
  show (Object t v) = show t <> " :: " <> show v
  show (Function args (A.BLOCK body) _) = "function(" <> intercalate ", " (map show args) <> ")\n{" <> intercalate "; " (map show body) <> "}"
  show _ = "no Show instance"

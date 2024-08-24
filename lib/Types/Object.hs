module Types.Object where

import           Control.Monad.Trans.State.Strict
import qualified Data.HashMap.Internal            as H
import           Data.List                        (intercalate)
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as M
import           Types.Ast                        as A
import           Types.Error                      (Error)

data Env = Env { isFinal :: Bool
               , heap    :: M.Map String Object
               }

data ObjectType = INTEGER_OBJ | BOOLEAN_OBJ | NULL_OBJ | STRING_OBJ | ARRAY_OBJ | FUNCTION_OBJ | BUILTIN_OBJ | HASH_OBJ | ANY_OBJ

instance Show ObjectType where
  show INTEGER_OBJ  = "INT"
  show BOOLEAN_OBJ  = "BOOl"
  show STRING_OBJ   = "STRING"
  show ARRAY_OBJ    = "ARRAY"
  show FUNCTION_OBJ = "FUNCTION"
  show HASH_OBJ     = "HASHMAP"
  show _            = "OBJECT"

instance Eq ObjectType where
  ANY_OBJ == _ = True
  _ == ANY_OBJ = True
  obj1 == obj2 = show obj1 == show obj2

data Object = Object { oType :: ObjectType
                     , value :: String
                     }
            | Array { oType :: ObjectType
                    , array :: [Object]
                    }
            | Function { oType :: ObjectType
                       , args  :: [A.Expr]
                       , body  :: A.Statement
                       , env   :: M.Map String Object
                       }
            | Builtin { oType :: ObjectType
                      , func  :: [Object] -> StateT Env (Either Error) Object
                      }
            | Hash { oType   :: ObjectType
                   , hashmap :: M.Map H.Hash (Object, Object)
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
  show (Function t args (A.BLOCK body) _) = show t <> " :: fn(" <> intercalate ", " (map show args) <> ")\n{" <> intercalate "; " (map show body) <> "}"
  show (Array t elements) = show t <> " :: " <> "[" <> intercalate ", " (map show elements) <> "]"
  show (Hash t hm) = show t <> " :: " <> "{" <> intercalate ", " (map (\(_, (k, v)) -> show k <> " : " <> show v) $ M.toList hm) <> "}"
  show _ = "no Show instance"

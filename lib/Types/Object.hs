module Types.Object where

import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State.Strict
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as M
import           Types.Error

data ObjectType = INTEGER_OBJ | STRING_OBJ | BOOL_OBJ | NULL_OBJ | ANY_OBJECT

instance Show ObjectType where
  show INTEGER_OBJ = "INT"
  show STRING_OBJ  = "STRING"
  show BOOL_OBJ    = "BOOl"
  show NULL_OBJ    = "NULL"
  show _           = "no show instance"

instance Eq ObjectType where
  ANY_OBJECT == _ = True
  _ == ANY_OBJECT = True
  o1 == o2        = show o1 == show o2

data Value = IntV Int
           | StringV String
           | BoolV Bool
           | NullV

instance Show Value where
  show (IntV i)    = show i
  show (StringV s) = show s
  show (BoolV b)   = if b then "True" else "False"
  show (NullV)     = "null"

data Object = Object { oType :: ObjectType
                     , value :: Value
                     }

instance Show Object where
  show (Object t v) = show t <> " :: " <> show v

data Env = Env { isReturn :: Bool
               , heap     :: Map String Object
               }
  deriving (Show)

initialEnv :: Env
initialEnv = Env False (M.fromList [("PI", Object INTEGER_OBJ (IntV 31415))])

type Program = ExceptT Error (StateT Env IO) Object

-- constants for Eval
trueConst, falseConst, nullConst :: Object
trueConst = Object BOOL_OBJ (BoolV True)
falseConst = Object BOOL_OBJ (BoolV False)
nullConst = Object NULL_OBJ NullV

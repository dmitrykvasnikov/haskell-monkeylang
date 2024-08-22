module Object where

import           Types.Ast as A

data ObjectType = INTEGER_OBJ | BOOLEAN_OBJ | NULL_OBJ | STRING_OBJ | RETURN_VALUE_OBJ deriving
  ( Eq
  , Show
  )

data Object = Object { obj   :: ObjectType
                     , value :: String
                     }
  deriving (Eq)

nullCONST, trueCONST, falseCONST :: Object
nullCONST = Object NULL_OBJ "null"
trueCONST = Object BOOLEAN_OBJ "true"
falseCONST = Object BOOLEAN_OBJ "false"

instance Show Object where
  show (Object o v) = "Type " <> show o <> " | Value " <> v

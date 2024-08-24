module Builtins where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Strict
import qualified Data.Map.Strict                  as M
import           Types.Error
import           Types.Object

constants, builtins :: M.Map String Object
builtins =
  M.fromList
    [ ( "len",
        Builtin
          { oType = BUILTIN_OBJ,
            func = \args -> case length args of
              0 -> lift $ Left $ EvalError "there is no arguments provided for 'len' function"
              1 ->
                let arg = head args
                 in case oType arg of
                      STRING_OBJ -> return $ Object INTEGER_OBJ $ show $ length $ value $ arg
                      ARRAY_OBJ -> return $ Object INTEGER_OBJ $ show $ length $ array $ arg
                      _ -> lift $ Left $ EvalError $ (show . oType $ arg) <> " is not supported in 'len' method"
              _ -> lift $ Left $ EvalError $ "only one argument is valid for 'len' function, but provided " <> (show $ length args) <> " arguments"
          }
      ),
      ( "info",
        Builtin
          { oType = BUILTIN_OBJ,
            func = \_ -> return $ Object STRING_OBJ "MonkeyLang Interpreter version Banana.0.0.1"
          }
      ),
      ( "min",
        Builtin
          { oType = BUILTIN_OBJ,
            func = \[arg1, arg2] -> typeCheck INTEGER_OBJ arg1 >> typeCheck INTEGER_OBJ arg2 >> if (read @Int $ value arg1) > (read @Int $ value arg2) then return arg2 else return arg1
          }
      ),
      ( "max",
        Builtin
          { oType = BUILTIN_OBJ,
            func = \[arg1, arg2] -> if (read @Int $ value arg1) < (read @Int $ value arg2) then return arg2 else return arg1
          }
      )
    ]
constants =
  M.fromList
    [ ("PI", Object INTEGER_OBJ "3.1415")
    ]

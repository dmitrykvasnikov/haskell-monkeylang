module Builtins where

<<<<<<< HEAD
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Strict
import qualified Data.Map.Strict                  as M
import           Types.Error
=======
import           Control.Monad.Trans.State.Strict
import qualified Data.Map.Strict                  as M
>>>>>>> 56b42f5cdd7989e178803f6f63186eef205d00cc
import           Types.Object

constants, builtins :: M.Map String Object
builtins =
  M.fromList
    [ ( "len",
        Builtin
<<<<<<< HEAD
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
=======
          { inp = [STRING_OBJ],
            out = INTEGER_OBJ,
            func = \[str] -> return $ Object INTEGER_OBJ $ show $ length $ value str
>>>>>>> 56b42f5cdd7989e178803f6f63186eef205d00cc
          }
      ),
      ( "info",
        Builtin
<<<<<<< HEAD
          { oType = BUILTIN_OBJ,
            func = \_ -> return $ Object STRING_OBJ "MonkeyLang Interpreter version Banana.0.0.1"
=======
          { inp = [],
            out = STRING_OBJ,
            func = \_ -> return $ Object STRING_OBJ "MonkeyLang Interpreter version Banana.0.1"
>>>>>>> 56b42f5cdd7989e178803f6f63186eef205d00cc
          }
      ),
      ( "min",
        Builtin
<<<<<<< HEAD
          { oType = BUILTIN_OBJ,
            func = \[arg1, arg2] -> typeCheck INTEGER_OBJ arg1 >> typeCheck INTEGER_OBJ arg2 >> if (read @Int $ value arg1) > (read @Int $ value arg2) then return arg2 else return arg1
=======
          { inp = [INTEGER_OBJ, INTEGER_OBJ],
            out = INTEGER_OBJ,
            func = \[arg1, arg2] -> if (read @Int $ value arg1) > (read @Int $ value arg2) then return arg2 else return arg1
>>>>>>> 56b42f5cdd7989e178803f6f63186eef205d00cc
          }
      ),
      ( "max",
        Builtin
<<<<<<< HEAD
          { oType = BUILTIN_OBJ,
=======
          { inp = [INTEGER_OBJ, INTEGER_OBJ],
            out = INTEGER_OBJ,
>>>>>>> 56b42f5cdd7989e178803f6f63186eef205d00cc
            func = \[arg1, arg2] -> if (read @Int $ value arg1) < (read @Int $ value arg2) then return arg2 else return arg1
          }
      )
    ]
constants =
  M.fromList
    [ ("PI", Object INTEGER_OBJ "3.1415")
    ]

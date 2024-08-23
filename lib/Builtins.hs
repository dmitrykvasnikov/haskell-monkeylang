module Builtins where

import           Control.Monad.Trans.State.Strict
import qualified Data.Map.Strict                  as M
import           Types.Object

constants, builtins :: M.Map String Object
builtins =
  M.fromList
    [ ( "len",
        Builtin
          { inp = [STRING_OBJ],
            out = INTEGER_OBJ,
            func = \[str] -> return $ Object INTEGER_OBJ $ show $ length $ value str
          }
      ),
      ( "info",
        Builtin
          { inp = [],
            out = STRING_OBJ,
            func = \_ -> return $ Object STRING_OBJ "MonkeyLang Interpreter version Banana.0.1"
          }
      ),
      ( "min",
        Builtin
          { inp = [INTEGER_OBJ, INTEGER_OBJ],
            out = INTEGER_OBJ,
            func = \[arg1, arg2] -> if (read @Int $ value arg1) > (read @Int $ value arg2) then return arg2 else return arg1
          }
      ),
      ( "max",
        Builtin
          { inp = [INTEGER_OBJ, INTEGER_OBJ],
            out = INTEGER_OBJ,
            func = \[arg1, arg2] -> if (read @Int $ value arg1) < (read @Int $ value arg2) then return arg2 else return arg1
          }
      )
    ]
constants =
  M.fromList
    [ ("PI", Object INTEGER_OBJ "3.1415")
    ]

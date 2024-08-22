module Builtins where

import qualified Data.Map.Strict as M
import           Types.Object

builtins :: M.Map String Object
builtins =
  M.fromList
    [ ( "len",
        Builtin
          { inp = [STRING_OBJ],
            out = INTEGER_OBJ,
            func = \[str] ->
              let l = length $ value str
               in Object INTEGER_OBJ (show l)
          }
      ),
      ( "time",
        Builtin
          { inp = [],
            out = INTEGER_OBJ,
            func = \_ -> Object INTEGER_OBJ "42"
          }
      )
    ]

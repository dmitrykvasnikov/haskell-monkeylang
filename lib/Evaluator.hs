module Evaluator where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Strict
import qualified Data.HashMap.Internal            as H
import           Data.List
import qualified Data.Map.Strict                  as M
import           Types.Ast                        as A
import           Types.Error
import           Types.Object
import           Types.Token                      as T

type OType = ObjectType

initialEnv :: Env
initialEnv = Env {isFinal = False, heap = constants}

type Eval a = StateT Env (Either Error) a

evalProgram :: A.Statement -> Either Error Object
evalProgram st = evalStateT (evalStatement ANY_OBJ st) initialEnv

evalStatement :: OType -> A.Statement -> Eval Object
evalStatement t (A.BLOCK sts) = evalBlockStatement t sts
evalStatement t (A.EXPRESSION expr) = evalExpression t expr
evalStatement t (A.RETURN expr) = evalReturnExpression t expr
evalStatement _ (A.LET (T.ID var) expr) = evalExpression ANY_OBJ expr >>= \val -> modify (\e -> e {heap = M.insert var val (heap e)}) >> return val
evalStatement _ _ = return nullCONST

evalBlockStatement :: OType -> [A.Statement] -> Eval Object
evalBlockStatement _ [] = return nullCONST
evalBlockStatement t [s] = evalStatement t s
evalBlockStatement t (s : sts) =
  evalStatement t s >>= \obj ->
    gets isFinal >>= \case
      True  -> return obj
      False -> evalBlockStatement t sts

evalExpression, evalReturnExpression :: OType -> A.Expr -> Eval Object
evalExpression t (A.VAR var) = getVarFromHeap var >>= typeCheck t
evalExpression t (A.NUM num) = typeCheck t (Object INTEGER_OBJ num)
evalExpression t (A.STRING str) = typeCheck t (Object STRING_OBJ str)
evalExpression t (A.BOOL bool) = typeCheck t (if bool == "True" then trueCONST else falseCONST)
evalExpression t (A.UNOP (T.MINUS) expr) = evalNegateExpression expr >>= typeCheck t
evalExpression t (A.UNOP (T.NOT) expr) = evalNotExpression expr >>= typeCheck t
evalExpression t (A.BINOP op expr1 expr2) = evalInfixExpression op expr1 expr2 >>= typeCheck t
evalExpression t (A.IF cond cons alt) = evalExpression BOOLEAN_OBJ cond >>= evalIfExpression cons alt >>= typeCheck t
evalExpression _ (A.FN args body) = gets heap >>= \env -> return $ Function FUNCTION_OBJ args body env
evalExpression _ (A.ARRAY elements) = traverse (evalExpression ANY_OBJ) elements >>= return . Array ARRAY_OBJ
evalExpression t (A.INDEX ind arr) = evalIndexExpression ind arr >>= typeCheck t
evalExpression t (A.CALL fn args) = evalCallExpresion fn args >>= typeCheck t
evalExpression t (A.HASH hashmap) = evalHashLiteral hashmap
evalExpression _ _ = return nullCONST
-- evalReturnExpression t expr = evalExpression t expr >>= \obj -> modify (\e -> e {isFinal = True}) >> return obj
evalReturnExpression t expr = modify (\e -> e {isFinal = True}) >> evalExpression t expr

evalNegateExpression, evalNotExpression :: A.Expr -> Eval Object
evalNegateExpression expr = (evalExpression INTEGER_OBJ expr) >>= \obj -> return $ obj {value = show . negate . read @Int . value $ obj}
evalNotExpression expr = (evalExpression BOOLEAN_OBJ expr) >>= \obj -> return $ notOp obj
  where
    notOp arg
      | arg == trueCONST = falseCONST
      | arg == falseCONST = trueCONST
      | arg == nullCONST = trueCONST
      | otherwise = falseCONST

evalInfixExpression :: T.Token -> A.Expr -> A.Expr -> Eval Object
evalInfixExpression op expr1 expr2
  | elem op [T.PLUS, T.MINUS, T.MULT, T.DIV] = do
      arg1 <- evalExpression INTEGER_OBJ expr1
      arg2 <- evalExpression INTEGER_OBJ expr2
      return $ Object INTEGER_OBJ (show $ (binop op) (read @Int $ value arg1) (read @Int $ value arg2))
  | op == T.CONCAT = do
      arg1 <- evalExpression STRING_OBJ expr1
      arg2 <- evalExpression STRING_OBJ expr2
      return $ Object STRING_OBJ (value arg1 ++ value arg2)
  | elem op [T.EQL, T.NOTEQL, T.GRT, T.LST, T.GRTEQL, T.LSTEQL] = do
      arg1 <- evalExpression ANY_OBJ expr1
      arg2 <- evalExpression ANY_OBJ expr2
      case (oType arg1) == (oType arg2) of
        True -> return $ boolToBoolean $ (comp op) (value arg1) (value arg2)
        False -> mkEvalError $ intercalate " " ["can not match types: ", show (oType arg1), "and", show (oType arg2), "in operation", show op]
  | otherwise = return nullCONST

evalIfExpression :: Statement -> Statement -> Object -> Eval Object
evalIfExpression cons alt cond = evalStatement ANY_OBJ (if cond == trueCONST then cons else alt)

evalHashLiteral :: M.Map A.Expr A.Expr -> Eval Object
evalHashLiteral hm = traverse (mkHashPair) (M.toList hm) >>= return . Hash HASH_OBJ . M.fromList
  where
    mkHashPair :: (A.Expr, A.Expr) -> Eval (H.Hash, (Object, Object))
    mkHashPair (k, v) = do
      key <- evalExpression ANY_OBJ k
      case (elem (oType key) [INTEGER_OBJ, BOOLEAN_OBJ, STRING_OBJ]) of
        False -> lift $ Left $ EvalError $ "not hashable type in hash : " <> show (oType key)
        True -> do
          let h = H.hash $ show key
          val <- evalExpression ANY_OBJ v
          return (h, (key, val))

evalIndexExpression :: A.Expr -> A.Expr -> Eval Object
evalIndexExpression indExpr objExpr = do
  obj <- evalExpression ANY_OBJ objExpr
  case oType obj of
    ARRAY_OBJ -> do
      indObj <- evalExpression INTEGER_OBJ indExpr
      let ind = read @Int $ value indObj
      let arr = array obj
      case (ind < 0) || (ind > (length arr) - 1) of
        True -> lift $ Left $ EvalError $ "index " <> show ind <> " out of bound"
        False -> return $ arr !! ind
    HASH_OBJ -> do
      ind <- evalExpression ANY_OBJ indExpr
      case (elem (oType ind) [INTEGER_OBJ, BOOLEAN_OBJ, STRING_OBJ]) of
        False -> mkEvalError $ "not hashable type in index :" <> show (oType ind)
        True -> do
          hm <- evalExpression HASH_OBJ objExpr
          case M.lookup (H.hash $ show ind) (hashmap hm) of
            Just (_, v) -> return v
            Nothing     -> return nullCONST
    t -> lift $ Left $ mkTypeError ARRAY_OBJ obj

--   let ind' = read @Int $ value ind
--   let arr' = array arr
--   case (ind' < 0) || (ind' > (length arr') - 1) of
--     True  -> lift $ Left $ EvalError $ "index " <> show ind' <> " out of bound"
--     False -> return $ arr' !! ind'

evalCallExpresion :: A.Expr -> [A.Expr] -> Eval Object
evalCallExpresion fn args = do
  closure <- gets heap
  function <- getFunctionForCall fn
  case function of
    Just (Function _ vars body env) -> do
      case (length vars) == (length args) of
        False -> mkEvalError $ "amount of passed and declared arguments is different in call of function \"" <> show fn <> "\""
        True -> do
          args' <- traverse (evalExpression ANY_OBJ) args
          let closure' = M.union (M.fromList $ zipWith (,) (map (\(A.VAR var) -> var) vars) args') $ M.union closure env
          let result = evalStateT (evalStatement ANY_OBJ body) (Env False closure')
          case result of
            Right obj -> return obj
            Left err  -> lift $ Left $ err
    _ -> case fn of
      (A.VAR var) -> case M.lookup var builtins of
        (Just builtin) -> traverse (evalExpression ANY_OBJ) args >>= func builtin
        _ -> mkEvalError $ show fn <> " is not a function."
      _ -> mkEvalError $ show fn <> " is not a function."

getFunctionForCall :: A.Expr -> Eval (Maybe Object)
getFunctionForCall expr =
  case expr of
    (A.FN _ _)  -> evalExpression ANY_OBJ expr >>= return . Just
    (A.VAR var) -> gets heap >>= return . M.lookup var
    _           -> lift $ Left $ EvalError $ show expr <> "is not a function"

-- helpers for Type Errors
mkTypeError :: ObjectType -> Object -> Error
mkTypeError t o = EvalError $ "type error, can't match type " <> (show $ oType o) <> " with expected type " <> show t <> "\n" <> show o

typeCheck :: ObjectType -> Object -> Eval Object
typeCheck t o = case t == (oType o) of
  True  -> return o
  False -> lift $ Left $ mkTypeError t o

mkEvalError :: String -> Eval Object
mkEvalError err = lift $ Left $ EvalError err

-- helper to conver bool value to Boolean object
boolToBoolean :: Bool -> Object
boolToBoolean True  = trueCONST
boolToBoolean False = falseCONST

binop :: (Num a, Integral a) => T.Token -> (a -> a -> a)
binop T.PLUS  = (+)
binop T.MINUS = (-)
binop T.MULT  = (*)
binop T.DIV   = div
binop _       = error "never gonna happen"

comp :: (Eq a, Ord a) => T.Token -> (a -> a -> Bool)
comp T.EQL    = (==)
comp T.NOTEQL = (/=)
comp T.GRT    = (>)
comp T.LST    = (<)
comp T.GRTEQL = (>=)
comp T.LSTEQL = (<=)
comp _        = error "never gonna happen"

getVarFromHeap :: String -> Eval Object
getVarFromHeap var = do
  env <- gets heap
  case M.lookup var env of
    Just val -> return val
    Nothing  -> mkEvalError $ "found no variable with name " <> show var

-- helper to check if object is Truthy
isTruthy :: Object -> Bool
isTruthy obj
  | obj == nullCONST = False
  | obj == falseCONST = False
  | otherwise = True

-- Built-in Constants and functions
constants, builtins :: M.Map String Object
builtins =
  M.fromList
    [ ( "len",
        Builtin
          { oType = BUILTIN_OBJ,
            func = \args -> do
              checkArgsCount "len" args 1
              let arg = head args
              case oType arg of
                STRING_OBJ -> return $ Object INTEGER_OBJ $ show $ length $ value $ arg
                ARRAY_OBJ -> return $ Object INTEGER_OBJ $ show $ length $ array $ arg
                _ -> mkEvalError $ (show . oType $ arg) <> " is not supported in 'len' method"
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
            func = \args ->
              do
                checkArgsCount "min" args 2
                let arg1 = args !! 0
                    arg2 = args !! 1
                typeCheck INTEGER_OBJ arg1 >> typeCheck INTEGER_OBJ arg2 >> if (read @Int $ value arg1) > (read @Int $ value arg2) then return arg2 else return arg1
          }
      ),
      ( "max",
        Builtin
          { oType = BUILTIN_OBJ,
            func = \args ->
              do
                checkArgsCount "man" args 2
                let arg1 = args !! 0
                    arg2 = args !! 1
                typeCheck INTEGER_OBJ arg1 >> typeCheck INTEGER_OBJ arg2 >> if (read @Int $ value arg1) < (read @Int $ value arg2) then return arg2 else return arg1
          }
      ),
      ( "first",
        Builtin
          { oType = BUILTIN_OBJ,
            func = \args -> do
              checkArgsCount "first" args 1
              let arr = head args
              typeCheck ARRAY_OBJ arr
              let arr' = array arr
              if (length arr') == 0 then return nullCONST else return $ head arr'
          }
      ),
      ( "last",
        Builtin
          { oType = BUILTIN_OBJ,
            func = \args -> do
              checkArgsCount "last" args 1
              let arr = head args
              typeCheck ARRAY_OBJ arr
              let arr' = array arr
              if (length arr') == 0 then return nullCONST else return $ last arr'
          }
      ),
      ( "tail",
        Builtin
          { oType = BUILTIN_OBJ,
            func = \args -> do
              checkArgsCount "tail" args 1
              let arr = head args
              typeCheck ARRAY_OBJ arr
              let arr' = array arr
              if (length arr') == 0 then return nullCONST else return $ Array ARRAY_OBJ $ tail arr'
          }
      ),
      ( "push",
        Builtin
          { oType = BUILTIN_OBJ,
            func = \args -> do
              checkArgsCount "push" args 2
              let arr = args !! 0
                  el = args !! 1
              typeCheck ARRAY_OBJ arr
              let arr' = array arr
              return $ Array ARRAY_OBJ $ arr' ++ [el]
          }
      ),
      ( "lookup",
        Builtin
          { oType = BUILTIN_OBJ,
            func = \args -> do
              checkArgsCount "lookup" args 2
              let ind = args !! 0
                  obj = args !! 1
              case (elem (oType ind) [INTEGER_OBJ, BOOLEAN_OBJ, STRING_OBJ]) of
                False -> lift $ Left $ EvalError $ "not hashable type in index :" <> show (oType ind)
                True -> case (oType obj) == HASH_OBJ of
                  False -> mkEvalError $ "index operator should be applyed to array of hashmap, but got " <> show (oType obj)
                  True -> case M.lookup (H.hash $ show ind) (hashmap obj) of
                    Nothing     -> return nullCONST
                    Just (_, v) -> return v
          }
      )
    ]
constants =
  M.fromList
    [ ("PI", Object INTEGER_OBJ "3.1415")
    ]

-- helper function of Built-in functions
checkArgsCount :: String -> [Object] -> Int -> Eval ()
checkArgsCount fn args c = case c == length args of
  True -> return ()
  False -> lift $ Left $ EvalError $ "wrong number of arguments for method " <> show fn <> ", got " <> show (length args) <> ", want " <> show c

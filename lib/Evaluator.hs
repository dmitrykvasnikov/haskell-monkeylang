module Evaluator where

import           Builtins
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Strict
import           Data.List
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as M
import           GHC.ExecutionStack               (Location (functionName))
import           Types.Ast                        as A
import           Types.Error
import           Types.Object
import           Types.Token                      as T

data Env = Env { isFinal :: Bool
               , heap    :: Map String Object
               }

type OType = ObjectType

initialEnv :: Env
initialEnv = Env {isFinal = False, heap = M.empty}

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
evalExpression _ (A.FN args body) = gets heap >>= \env -> return $ Function args body env
evalExpression t (A.CALL fn args) = evalCallExpresion fn args >>= typeCheck t
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
        False -> lift $ Left $ EvalError $ intercalate " " ["can not match types: ", show (oType arg1), "and", show (oType arg2), "in operation", show op]
  | otherwise = return nullCONST

evalIfExpression :: Statement -> Statement -> Object -> Eval Object
evalIfExpression cons alt cond = evalStatement ANY_OBJ (if cond == trueCONST then cons else alt)

evalCallExpresion :: A.Expr -> [A.Expr] -> Eval Object
evalCallExpresion fn args = do
  closure <- gets heap
  function <- getFunctionForCall fn
  case function of
    Just (Function vars body env) -> do
      case (length vars) == (length args) of
        False -> lift $ Left $ EvalError $ "amount of passed and declared variables is different in call of function \"" <> show fn <> "\""
        True -> do
          args' <- traverse (evalExpression ANY_OBJ) args
          let closure' = M.union (M.fromList $ zipWith (,) (map (\(A.VAR var) -> var) vars) args') $ M.union closure env
          let result = evalStateT (evalStatement ANY_OBJ body) (Env False closure')
          case result of
            Right obj -> return obj
            Left err  -> lift $ Left $ err
    _ -> case fn of
      (A.VAR var) -> case M.lookup var builtins of
        (Just builtin) -> do
          case (length args) == (length $ inp builtin) of
            False -> lift $ Left $ EvalError $ "amount of passed and declared variables is different in call of function \"" <> show fn <> "\""
            True -> sequence (zipWith evalExpression (inp builtin) args) >>= return . func builtin
        _ -> lift $ Left $ EvalError $ show fn <> " is not a function."
      _ -> lift $ Left $ EvalError $ show fn <> " is not a function."

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
typeCheck t o =
  let b = (t == oType o)
   in case b of
        True  -> return o
        False -> lift $ Left $ mkTypeError t o

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
    Nothing -> lift $ Left $ EvalError $ "found no variable with name " <> show var

-- helper to check if object is Truthy
isTruthy :: Object -> Bool
isTruthy obj
  | obj == nullCONST = False
  | obj == falseCONST = False
  | otherwise = True

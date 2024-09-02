module Eval where

import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.Except       (runExceptT, throwE)
import           Control.Monad.Trans.State.Strict (gets, modify, runStateT)
import           Data.HashMap.Internal
import           Data.Map                         (Map)
import qualified Data.Map                         as M
import           Parser
import           Types.Ast
import           Types.Error
import           Types.Object

evalProgram :: Statement -> Program
evalProgram = evalStatement

evalStatement :: Statement -> Program
evalStatement (LetS p1 p2 i e)   = evalLetS p1 p2 i e
evalStatement (BlockS _ _ sts)   = evalBlockS sts
evalStatement (ExprS p1 p2 expr) = evalE p1 p2 expr
evalStatement s                  = makeEvalError 0 0 $ "error " <> show s

evalLetS :: Int -> Int -> Expr -> Expr -> Program
evalLetS p1 p2 (IdE var) e = evalE p1 p2 e >>= \val -> (lift . modify $ (\e -> e {heap = M.insert var val (heap e)})) >> return nullConst

evalBlockS :: [Statement] -> Program
evalBlockS [] = return nullConst
evalBlockS [s] = evalStatement s
evalBlockS (s : ss) = do
  o <- evalStatement s
  b <- lift . gets $ isReturn
  if b then return o else evalBlockS ss

evalE :: Int -> Int -> Expr -> Program
evalE _ _ (IntE i) = return $ Object INTEGER_OBJ (IntV i)
evalE _ _ (StringE s) = return $ Object STRING_OBJ (StringV s)
evalE _ _ (BoolE b) = return $ if b then trueConst else falseConst
evalE p1 p2 (IdE i) = do
  e <- lift . gets $ heap
  case M.lookup i e of
    Just o  -> return o
    Nothing -> makeEvalError p1 p2 $ "unknow identifier : '" <> i <> "'"

checkType :: Int -> Int -> ObjectType -> Object -> Program
checkType p1 p2 t o = do
  let ot = oType o
   in if t == ot
        then return o
        else makeEvalError p1 p2 $ "type error: expect " <> show t <> " but got " <> show o

makeEvalError :: Int -> Int -> String -> Program
makeEvalError p1 p2 msg = throwE . EvalError p1 p2 $ msg

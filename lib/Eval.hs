module Eval where

import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.Except       (runExceptT, throwE)
import           Control.Monad.Trans.State.Strict (gets, modify, runStateT)
import           Data.Data                        (constrType)
import           Data.HashMap.Internal
import           Data.Map                         (Map)
import qualified Data.Map                         as M
import qualified Data.Text                        as T
import           Debug.Trace
import           Input
import           Types.Ast
import           Types.Error
import           Types.Object
import           Types.Token

evalProgram :: Stream Object
evalProgram = (lift . gets $ program) >>= run
  where
    run :: [Statement] -> Stream Object
    run []        = return nullConst
    run [s]       = evalStatement s >>= return
    run (s : sts) = evalStatement s >> run sts

evalStatement :: Statement -> Stream Object
evalStatement s@(LetS _ _ i e)   = sP s >> evalLetS i e
evalStatement s@(BlockS _ _ sts) = sP s >> evalBlockS sts
evalStatement s@(ExprS _ _ expr) = sP s >> evalE expr
evalStatement s                  = makeEvalError $ "error " <> show s

evalLetS :: Expr -> Expr -> Stream Object
evalLetS (IdE var) e = evalE e >>= \val -> (lift . modify $ (\e -> e {heap = M.insert var val (heap e)})) >> return nullConst

evalBlockS :: [Statement] -> Stream Object
evalBlockS []       = return nullConst
evalBlockS [s]      = evalStatement s
evalBlockS (s : ss) = evalStatement s >> evalBlockS ss

evalE, evalNotE, evalNegateE :: Expr -> Stream Object
evalE (IntE i) = return $ Object INTEGER_OBJ (IntV i)
evalE (StringE s) = return $ Object STRING_OBJ (StringV s)
evalE (BoolE b) = return $ if b then trueConst else falseConst
evalE (IdE i) = do
  e <- lift . gets $ heap
  case M.lookup i e of
    Just o  -> return o
    Nothing -> makeEvalError $ "unknown identifier : '" <> i <> "'"
evalE (UnOpE op expr) = do
  let action
        | op == NOT = evalNotE expr
        | op == MINUS = evalNegateE expr
        | otherwise = makeEvalError "Unsupported unary operation"
  action
evalE (BinOpE op e1 e2) = evalInfixE op e1 e2
evalE _ = makeEvalError "This error should have never happens"
evalNotE e = evalE e >>= checkType BOOL_OBJ >>= \b -> if b == trueConst then return falseConst else return trueConst
evalNegateE e = evalE e >>= checkType INTEGER_OBJ >>= \(Object _ (IntV n)) -> return $ Object INTEGER_OBJ (IntV $ negate n)

evalInfixE :: Token -> Expr -> Expr -> Stream Object
evalInfixE op e1 e2
  | elem op [PLUS, MINUS, MULT, DIV] =
      do
        evalE e1 >>= checkType INTEGER_OBJ
        >>= \(Object _ (IntV n1)) ->
          evalE e2
            >>= checkType INTEGER_OBJ
            >>= \(Object _ (IntV n2)) -> return $ Object INTEGER_OBJ (IntV $ (mathop op) n1 n2)
  | elem op [GRT, LST, NOTEQL, GRTEQL, LSTEQL, EQL] = do
      e1' <- evalE e1
      e2' <- evalE e2
      let o1 = oType e1'
          o2 = oType e2'
      if all (flip elem [INTEGER_OBJ, BOOL_OBJ, STRING_OBJ]) [o1, o2]
        then
          if o1 == o2
            then case (value e1', value e2') of
              (IntV i1, IntV i2) -> return . boolToObject $ (compop op) i1 i2
              (StringV s1, StringV s2) -> return . boolToObject $ (compop op) s1 s2
              (BoolV b1, BoolV b2) -> return . boolToObject $ (compop op) b1 b2
            else makeEvalError $ "can not compare differnet types: " <> show o1 <> " and " <> show o2
        else makeEvalError "compare operation must be applied to INT, BOOL or STRING arguments"
  | elem op [AND, OR] =
      evalE e1 >>= checkType BOOL_OBJ >>= \e1 ->
        evalE e2 >>= checkType BOOL_OBJ >>= \e2 ->
          case op of
            AND -> if e1 == falseConst then return e1 else return e2
            OR  -> if e1 == trueConst then return e1 else return e2
  | otherwise = makeEvalError $ "'" <> "' is not an infix operator"
  where
    mathop PLUS  = (+)
    mathop MINUS = (-)
    mathop MULT  = (*)
    mathop DIV   = div
    compop LST    = (<)
    compop GRT    = (>)
    compop NOTEQL = (/=)
    compop LSTEQL = (<=)
    compop GRTEQL = (>=)
    compop EQL    = (==)

checkType :: ObjectType -> Object -> Stream Object
checkType t o = do
  let ot = oType o
   in if t == ot
        then return o
        else makeEvalError $ "type error: expect " <> show t <> " but got " <> show o

-- set and get first and last lines of current statement
sP :: Statement -> Stream ()
sP st = lift . modify $ (\s -> s {statementPos = go st})
  where
    go (BlockS b e _)  = (b, e)
    go (LetS b e _ _)  = (b, e)
    go (ReturnS b e _) = (b, e)
    go (ExprS b e _)   = (b, e)

gP :: Stream (Col, Col)
gP = lift . gets $ statementPos

boolToObject :: Bool -> Object
boolToObject True  = trueConst
boolToObject False = falseConst

makeEvalError :: String -> Stream Object
makeEvalError msg = do
  (b, e) <- gP
  src <- lift . gets $ input
  let begin = T.take (e - b + 1) $ T.drop b src
      end = T.takeWhile (/= '\n') (T.drop (e + 1) src)
  traceShow (show b <> "," <> show e) $ throwE . EvalError msg $ T.unpack $ begin <> end

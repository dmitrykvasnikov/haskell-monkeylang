module Evaluator where

import           Control.Monad.Trans.State.Strict
import           Object
import           Types.Ast                        as A
import           Types.Token                      as T

data Env = Env { returnObject :: Object
               , isFinal      :: Bool
               }

env = Env {returnObject = nullCONST, isFinal = False}

ev :: A.Statement -> State Env [Object]
ev (A.BLOCK sts) = traverse go sts
  where
    go st = do
      let ob = evalStatement st
      let isReturn = case st of
            (A.RETURN _) -> True
            _            -> False
      if isReturn
        then modify (\e -> e {isFinal = True, returnObject = ob}) >> return ob
        else gets isFinal >>= \b -> if b then return ob else modify (\e -> e {returnObject = ob}) >> return ob

evalProgram :: A.Statement -> Object
evalProgram (A.BLOCK sts) = foldl' eval' nullCONST sts
evalProgram st            = evalStatement st

evalStatement :: A.Statement -> Object
evalStatement (A.EXPRESSION expr) = evalExpression expr
evalStatement (A.RETURN expr)     = evalExpression expr

evalExpression :: A.Expr -> Object
evalExpression (A.NUM num) = Object INTEGER_OBJ num
evalExpression (A.BOOL bool) = if bool == "True" then trueCONST else falseCONST
evalExpression (A.STRING string) = Object STRING_OBJ string
evalExpression (UNOP op expr)
  | op == T.NOT = notOperatorEval (evalExpression expr)
  | op == T.MINUS = negateOperatorEval (evalExpression expr)
evalExpression (A.BINOP op l r) = infixOperatorEval op (evalExpression l) (evalExpression r)
evalExpression (A.IF cond cons al) = ifExpressionEval (evalExpression cond) cons al
evalExpression _ = Object NULL_OBJ "Null"

notOperatorEval :: Object -> Object
notOperatorEval arg
  | arg == trueCONST = falseCONST
  | arg == falseCONST = trueCONST
  | arg == nullCONST = trueCONST
  | otherwise = falseCONST

negateOperatorEval :: Object -> Object
negateOperatorEval arg
  | obj arg == INTEGER_OBJ = Object INTEGER_OBJ (show . negate . read @Int $ value arg)
  | otherwise = nullCONST

infixOperatorEval :: Token -> Object -> Object -> Object
infixOperatorEval op l r
  | elem op [T.PLUS, T.MINUS, T.MULT, T.DIV]
      && (obj l) == INTEGER_OBJ
      && (obj l) == INTEGER_OBJ =
      Object INTEGER_OBJ (show $ (binop op) (read @Int $ value l) (read @Int $ value r))
  | elem op [T.EQL, T.NOTEQL, T.GRT, T.LST, T.GRTEQL, T.LSTEQL]
      && (obj l) == (obj r) =
      boolToBoolean $ (comp op) (value l) (value r)
  | otherwise = nullCONST
  where
    binop T.PLUS  = (+)
    binop T.MINUS = (-)
    binop T.MULT  = (*)
    binop T.DIV   = div
    binop _       = error "never gonna happen"
    comp T.EQL    = (==)
    comp T.NOTEQL = (/=)
    comp T.GRT    = (>)
    comp T.LST    = (<)
    comp T.GRTEQL = (>=)
    comp T.LSTEQL = (<=)
    comp _        = error "never gonna happen"

ifExpressionEval :: Object -> Statement -> Statement -> Object
ifExpressionEval cond cons alt
  | cond == trueCONST = evalProgram cons
  | cond == falseCONST = evalProgram alt
  | otherwise = error "Evaluation error: not a bool result in IF condition"

-- helper to conver bool value to Boolean object
boolToBoolean :: Bool -> Object
boolToBoolean True  = trueCONST
boolToBoolean False = falseCONST

-- helper to check if object is Truthy
isTruthy :: Object -> Bool
isTruthy obj
  | obj == nullCONST = False
  | obj == falseCONST = False
  | otherwise = True

-- helper for fold of list of statements
eval' :: Object -> A.Statement -> Object
eval' ob st
  | obj ob == RETURN_VALUE_OBJ = ob
  | ob == nullCONST = evalStatement st
  | otherwise =
      let newObject = evalStatement st
       in if newObject /= nullCONST then newObject else ob

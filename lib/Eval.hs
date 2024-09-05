module Eval where

import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.Except       (throwE)
import           Control.Monad.Trans.State.Strict (get, gets, modify, put)
import qualified Data.HashMap.Internal            as H
import qualified Data.Map                         as M
import qualified Data.Text                        as T
import           Debug.Trace
import           Input
import           Types.Ast
import           Types.Error
import           Types.Object
import           Types.Token

type Eval a = Stream Object a

evalProgram :: Eval Object
evalProgram =
  let newHeap = M.fromList . map builtinToHeap $ builtins
   in (lift . modify $ (\e -> e {heap = M.union newHeap (heap e)})) >> (lift . gets $ program) >>= run

run :: [Statement] -> Eval Object
run [] = return nullConst
run [s] = evalStatement s >>= return
run (s : sts) = evalStatement s >>= \o -> ifReturn >>= \r -> if r then return o else run sts

evalStatement :: Statement -> Eval Object
evalStatement s@(LetS _ _ i e) = sP s >> evalLetS i e
evalStatement s@(ReturnS _ _ e) = sP s >> evalE e >>= \r -> (lift . modify $ (\s -> s {isReturn = True})) >> return r
evalStatement s@(BlockS _ _ sts) = sP s >> evalBlockS sts
evalStatement s@(ExprS _ _ expr) = sP s >> evalE expr

evalLetS :: Expr -> Expr -> Eval Object
evalLetS (IdE var) e = do
  checkRestricted var
  d <- lift . gets $ declaredVars
  case elem var d of
    True -> makeEvalError $ "variable '" <> var <> "' already declared"
    False -> evalE e >>= \val -> (lift . modify $ (\s -> s {declaredVars = (declaredVars s) ++ [var], heap = M.insert var val (heap s)})) >> return nullConst
evalLetS e _ = makeEvalError $ "in the left part of let expression should be ad identifier, but got " <> show e

evalBlockS :: [Statement] -> Eval Object
evalBlockS [] = return nullConst
evalBlockS [s] = evalStatement s
evalBlockS (s : ss) = evalStatement s >>= \o -> ifReturn >>= \r -> if r then return o else evalBlockS ss

evalE, evalNotE, evalNegateE :: Expr -> Eval Object
evalE (IntE i) = return $ Object INTEGER_OBJ (IntV i)
evalE (StringE s) = return $ Object STRING_OBJ (StringV s)
evalE (BoolE b) = return $ if b then trueConst else falseConst
evalE (IdE i) = do
  e <- lift . gets $ heap
  case M.lookup i e of
    Just o  -> return o
    Nothing -> makeEvalError $ "unknown identifier : '" <> i <> "'"
evalE (ArrayE arr) = traverse evalE arr >>= return . Object ARRAY_OBJ . ArrayV
evalE (HashE hs) = traverse makeHashPair (M.toList hs) >>= return . Object HASH_MAP . HashV . M.fromList
  where
    makeHashPair :: (Expr, Expr) -> Eval (H.Hash, (Object, Object))
    makeHashPair (keyE, valueE) = do
      key <- evalE keyE
      case elem (oType key) [INTEGER_OBJ, BOOL_OBJ, STRING_OBJ] of
        True -> evalE valueE >>= \value -> return (H.hash $ show key, (key, value))
        False -> getSource >>= throwE . EvalError "only INT, BOOL and STRING types could be keys in hashmap"
evalE (UnOpE op expr) = do
  let action
        | op == NOT = evalNotE expr
        | op == MINUS = evalNegateE expr
        | otherwise = makeEvalError "Unsupported unary operation"
  action
evalE (BinOpE op e1 e2) = evalInfixE op e1 e2
evalE (IfE cond t e) = evalE cond >>= \o -> isFalsy o >>= \b -> if b then (evalStatement e) else (evalStatement t)
evalE (IndexE expr ind) = do
  obj <- evalE expr
  case obj of
    (Object ARRAY_OBJ (ArrayV arr)) -> do
      Object _ (IntV ind') <- (evalE ind >>= checkType INTEGER_OBJ)
      case (ind' >= 0) && (ind' < length arr) of
        True  -> return $ arr !! ind'
        False -> makeEvalError $ "index '" <> show ind' <> "' out of bound"
    (Object HASH_MAP (HashV hs)) -> do
      key <- evalE ind
      case elem (oType key) [INTEGER_OBJ, BOOL_OBJ, STRING_OBJ] of
        True -> return $ maybe nullConst (\(_, v) -> v) (M.lookup (H.hash $ show key) hs)
        False -> getSource >>= throwE . EvalError "only INT, BOOL and STRING types could be keys in hashmap"
    _ -> makeEvalError "only Array and HashMap supports index operation"
evalE (FnE args body) = (lift . gets $ heap) >>= return . Object FUNCTION_OBJ . FnV (map (\(IdE var) -> var) args) body
evalE (CallE fn args) = do
  evalE fn >>= \case
    Object FUNCTION_OBJ (FnV params (BlockS _ _ body) closure) ->
      case length params /= length args of
        True -> makeEvalError "amount of parameters and arguments does not match"
        False -> do
          env <- lift get
          argsO <- traverse evalE args
          -- order of (heap env) and closure are important for correct work of recursion
          let newHeap = M.union (M.fromList $ zip params argsO) $ M.union (heap env) closure
          lift . modify $ (\e -> e {heap = newHeap, program = body, declaredVars = []})
          res <- run body
          d <- lift . gets $ declaredVars
          h <- (M.filterWithKey (\k _ -> not . elem k $ d)) <$> (lift . gets $ heap)
          lift . put $ env {heap = M.union h (heap env)}
          return res
    Object BUILTIN_OBJ (BuiltinV (Builtin _ builtin)) -> traverse evalE args >>= builtin
    _ -> makeEvalError $ show fn <> " is not a function"
evalE _ = makeEvalError "This error should have never happens"
evalNotE e = evalE e >>= checkType BOOL_OBJ >>= \b -> if b == trueConst then return falseConst else return trueConst
evalNegateE e = evalE e >>= checkType INTEGER_OBJ >>= \(Object _ (IntV n)) -> return $ Object INTEGER_OBJ (IntV $ negate n)

evalInfixE :: Token -> Expr -> Expr -> Eval Object
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
            else makeEvalError $ "can not compare different types: " <> show o1 <> " and " <> show o2
        else makeEvalError "compare operation must be applied to INT, BOOL or STRING arguments"
  | elem op [AND, OR] =
      evalE e1 >>= checkType BOOL_OBJ >>= \e1 ->
        evalE e2 >>= checkType BOOL_OBJ >>= \e2 ->
          case op of
            AND -> if e1 == falseConst then return e1 else return e2
            OR  -> if e1 == trueConst then return e1 else return e2
  | op == CONCAT =
      evalE e1 >>= checkType STRING_OBJ >>= \(Object STRING_OBJ (StringV s1)) ->
        evalE e2 >>= checkType STRING_OBJ >>= \(Object STRING_OBJ (StringV s2)) -> return $ Object STRING_OBJ (StringV $ s1 ++ s2)
  | op == ASSIGN =
      case e1 of
        (IdE var) -> do
          (M.lookup var) <$> (lift . gets $ heap) >>= \case
            Just v -> evalE e2 >>= \val -> (lift . modify $ (\s -> s {heap = M.insert var val (heap s)})) >> return nullConst
            Nothing -> makeEvalError $ "undeclared identifier '" <> var <> "'"
        _ -> makeEvalError $ "in the left part of let expression should be ad identifier, but got " <> show e1
  | otherwise = makeEvalError $ "'" <> "' is not an infix operator"
  where
    mathop PLUS  = (+)
    mathop MINUS = (-)
    mathop MULT  = (*)
    mathop DIV   = div
    mathop _     = todo
    compop LST    = (<)
    compop GRT    = (>)
    compop NOTEQL = (/=)
    compop LSTEQL = (<=)
    compop GRTEQL = (>=)
    compop EQL    = (==)
    compop _      = todo

checkType :: ObjectType -> Object -> Eval Object
checkType t o = do
  let ot = oType o
   in if t == ot
        then return o
        else makeEvalError $ "type error: expect " <> show t <> " but got " <> show o

ifReturn :: Eval Bool
ifReturn = lift . gets $ isReturn

-- set and get first and last lines of current statement
sP :: Statement -> Eval ()
sP st = lift . modify $ (\s -> s {statementPos = go st})
  where
    go (BlockS b e _)  = (b, e)
    go (LetS b e _ _)  = (b, e)
    go (ReturnS b e _) = (b, e)
    go (ExprS b e _)   = (b, e)

gP :: Eval (Col, Col)
gP = lift . gets $ statementPos

isFalsy :: Object -> Eval Bool
isFalsy o
  | o == nullConst = return True
  | o == falseConst = return True
  | otherwise = return False

boolToObject :: Bool -> Object
boolToObject True  = trueConst
boolToObject False = falseConst

makeEvalError :: String -> Eval Object
makeEvalError msg = getSource >>= throwE . EvalError msg

getSource :: Eval String
getSource = do
  (b, e) <- gP
  src <- lift . gets $ input
  let begin = T.take (e - b + 1) $ T.drop b src
      end = T.takeWhile (/= '\n') (T.drop (e + 1) src)
  return $ T.unpack $ begin <> end

-- Map of builtin functions
-- if you add new builtin - don't forget to include the name of it to restricted list in Types.Token
-- builtins doesn't get separate environment and runs in current state
builtins :: [(String, [Object] -> Eval Object)]
builtins =
  [ ( "length",
      \args -> do
        checkArgsAmount args 1
        case head args of
          (Object ARRAY_OBJ (ArrayV arr)) -> return . Object INTEGER_OBJ . IntV $ length arr
          (Object STRING_OBJ (StringV str)) -> return . Object INTEGER_OBJ . IntV $ length str
          _ -> makeEvalError "'length' supports only array and string arguments"
    ),
    ( "first",
      \args -> do
        checkArgsAmount args 1
        case head args of
          (Object ARRAY_OBJ (ArrayV arr)) -> return $ if arr == [] then nullConst else head arr
          _ -> makeEvalError "'first' supports only array argument"
    ),
    ( "last",
      \args -> do
        checkArgsAmount args 1
        case head args of
          (Object ARRAY_OBJ (ArrayV arr)) -> return $ if arr == [] then nullConst else last arr
          _ -> makeEvalError "'last' supports only array argument"
    ),
    ( "rest",
      \args -> do
        checkArgsAmount args 1
        case head args of
          (Object ARRAY_OBJ (ArrayV arr)) -> return $ if arr == [] then nullConst else Object ARRAY_OBJ (ArrayV $ tail arr)
          _ -> makeEvalError "'rest' supports only array argument"
    ),
    ( "push",
      \args -> do
        checkArgsAmount args 2
        case head args of
          (Object ARRAY_OBJ (ArrayV arr)) -> return $ Object ARRAY_OBJ (ArrayV $ (arr ++ [args !! 1]))
          _ -> makeEvalError "'push' supports only array argument"
    )
  ]

builtinToHeap :: (String, [Object] -> Stream Object Object) -> (String, Object)
builtinToHeap (s, f) = (s, Object BUILTIN_OBJ (BuiltinV $ Builtin s f))

-- check amount of arguments passed to builtin function
checkArgsAmount :: [Object] -> Int -> Eval Object
checkArgsAmount obs i = case i /= length obs of
  True -> makeEvalError $ "wrong amoutn of argument, need " <> show i <> ", got " <> (show $ length obs)
  False -> return nullConst

-- check if ID name is a restricted (one of builtin names)
checkRestricted :: String -> Eval Object
checkRestricted var = case elem var restricted of
  True -> makeEvalError $ "'" <> var <> "' is prohibited to use as identifier, it's a name of builtin function"
  False -> return nullConst

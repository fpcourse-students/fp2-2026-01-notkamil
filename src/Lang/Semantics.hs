module Lang.Semantics where

import Control.Applicative (asum)
import Control.Monad (foldM, zipWithM)
import Data.Foldable (fold)
import Data.Map (Map, insert, (!?))
import Data.Map qualified as Map
import Data.Maybe
import Lang.Syntax
import MetaUtils

-- | Значения, в которые вычисляются выражения.
data Value = Number Int | Object Ctor [Value]
  deriving (Eq)

instance Show Value where
  show = \case
    Number value -> show value
    Object ctor values
      | null values -> ctor
      | otherwise -> "(" <> ctor <> " " <> unwords (show <$> values) <> ")"

-- | Булевы константы: объекты без полей.
trueValue, falseValue :: Value
trueValue = Object "True" []
falseValue = Object "False" []

-- | Состояние программы - это просто словарь из имён переменных в их значения.
-- Ознакомьтесь с интерфейсом Data.Map с помощью Hoogle.
type Env = Map Name Value

eval :: Expr -> Value
eval expr = evalExpr expr Map.empty

-- | Интерпретатор выражений.
evalExpr :: Expr -> Env -> Value
evalExpr expr env = case expr of
  Const value -> Number value
  Var name -> case env !? name of
    Nothing -> error $ "ERROR: Undefined variable '" <> name <> "'"
    Just value -> value
  BinOp op lhs rhs -> evalBinOp op (evalExpr lhs env) (evalExpr rhs env)
  Constr ctor exprs -> Object ctor $ map (`evalExpr` env) exprs
  Match ex bs -> evalMatch (evalExpr ex env) bs env

evalMatch :: Value -> [Branch] -> Env -> Value
evalMatch v bs env = case res of
  Nothing -> error "ERROR: no pattern has matched"
  Just val -> val
  where
    res =
      foldl
        ( \accum nb -> case accum of
            Just val -> Just val
            Nothing -> evalMatchBranch v nb env
        )
        Nothing
        bs

evalMatchBranch :: Value -> Branch -> Env -> Maybe Value
evalMatchBranch v (Branch pat expr) env = case matchPattern v pat env of
  Nothing -> Nothing
  Just aenv -> Just $ evalExpr expr aenv

matchPattern :: Value -> Pattern -> Env -> Maybe Env
matchPattern v p e = case (v, p) of
  (Number n, ConstPat c) -> if n == c then Just e else Nothing
  (_, VarPat var) -> Just $ Map.insert var v e
  (Object ctor vs, CtorPat pctor pats) -> if ctor == pctor && length vs == length pats then ctorRes vs pats else Nothing
  _ -> Nothing
  where
    ctorRes vs pats =
      foldl
        ( \accum nv -> case (accum, nv) of
            (Nothing, _) -> Nothing
            (Just aenv, (val, pat)) -> matchPattern val pat aenv
        )
        (Just e)
        (zip vs pats)

-- | Интерпретатор бинарных операций.
evalBinOp :: BinOp -> Value -> Value -> Value
evalBinOp op = case op of
  Plus -> wrapNum (+)
  Minus -> wrapNum (-)
  Mult -> wrapNum (*)
  Div -> wrapNum (div)
  Less -> wrapBool (<)
  LessEq -> wrapBool (<=)
  Equal -> wrapBool (==)
  where
    wrapNum op lhs rhs = Number $ unpackNum lhs `op` unpackNum rhs
    wrapBool op lhs rhs = if unpackNum lhs `op` unpackNum rhs then trueValue else falseValue
    unpackNum = \case
      Number value -> value
      obj@Object {} -> error $ "TYPE_ERROR: Expected number, got object " <> show obj

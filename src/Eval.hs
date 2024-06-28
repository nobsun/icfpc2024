{-# LANGUAGE OverloadedStrings #-}
module Eval where

import Data.Char (ord)
import Data.List (foldl')
import qualified Data.ByteString.Char8 as BS
import Debug.Trace (trace)

import Parser (parseExpr)
import Expr
f $? x = let v = f x
             msg = "{- " ++ show x ++ " => " ++ show v ++ " -}"
         in trace msg v



type Env = [(Var, Expr)]

eval :: Env -> Expr -> Either String (Expr, Env)
eval env e = case e of
  EBool b -> Right (e, env)
  EInt i  -> Right (e, env)
  EStr s  -> Right (e, env)
  EUnary op e -> unary env op e
  EBinary op e1 e2 -> binary env op e1 e2
  EIf c t e -> do
    (c', env') <- eval env c
    case c' of
      EBool True  -> eval env' t
      EBool False -> eval env' e
      _           -> Left "If condition is not a boolean"
  ELambda v b -> Right (ELambda v b, env) -- TODO
  EVar v      -> maybe (Left msg) (eval env) $ lookup v env
    where
      msg = "Variable " ++ show v ++ " not found"

unary :: Env -> UOp -> Expr -> Either String (Expr, Env)
unary env Not e = do
  (e', env') <- eval env e
  case e' of
    EBool b -> Right (EBool $ not b, env')
    _       -> Left "Not applied to non-boolean"
unary env Neg e = do
  (e', env') <- eval env e
  case e' of
    EInt i -> Right (EInt $ negate i, env')
    _      -> Left "Neg applied to non-integer"
unary env StrToInt e = do
  (e', env') <- eval env e
  case e' of
    EStr cs -> Right (EInt i, env')
      where
        cs' = tail $ BS.unpack $ encodeStr cs
        i = fromIntegral $ foldl' (\acc c -> acc * 94 + ord c - ord '!') 0 cs'
    _       -> Left "StrToInt applied to non-string"
unary env IntToStr e = do
  (e', env') <- eval env e
  case e' of
    EInt i -> Right (EStr $ cultToHuman $ encodeBase94 i, env')
    _      -> Left "IntToStr applied to non-integer"

binary :: Env -> BinOp -> Expr -> Expr -> Either String (Expr, Env)
binary env Add e1 e2 = do
  (e1', env')  <- eval env e1
  (e2', env'') <- eval env' e2
  case (e1', e2') of
    (EInt i1, EInt i2) -> Right (EInt $ i1 + i2, env'')
    _                  -> Left "Add applied to non-integers"
binary env Sub e1 e2 = do
  (e1', env')  <- eval env e1
  (e2', env'') <- eval env' e2
  case (e1', e2') of
    (EInt i1, EInt i2) -> Right (EInt $ i1 - i2, env'')
    _                  -> Left "Sub applied to non-integers"
binary env Mult e1 e2 = do
  (e1', env')  <- eval env e1
  (e2', env'') <- eval env' e2
  case (e1', e2') of
    (EInt i1, EInt i2) -> Right (EInt $ i1 * i2, env'')
    _                  -> Left "Mult applied to non-integers"
binary env Div e1 e2 = do
  (e1', env')  <- eval env e1
  (e2', env'') <- eval env' e2
  case (e1', e2') of
    (EInt i1, EInt i2) -> Right (EInt $ i1 `quot` i2, env'')
    _                  -> Left "Div applied to non-integers"
binary env Mod e1 e2 = do
  (e1', env')  <- eval env e1
  (e2', env'') <- eval env' e2
  case (e1', e2') of
    (EInt i1, EInt i2) -> Right (EInt $ i1 `rem` i2, env'')
    _                  -> Left "Mod applied to non-integers"
binary env Lt e1 e2 = do
  (e1', env')  <- eval env e1
  (e2', env'') <- eval env' e2
  case (e1', e2') of
    (EInt i1, EInt i2) -> Right (EBool $ i1 < i2, env'')
    _                  -> Left "Lt applied to non-integers"
binary env Gt e1 e2 = do
  (e1', env')  <- eval env e1
  (e2', env'') <- eval env' e2
  case (e1', e2') of
    (EInt i1, EInt i2) -> Right (EBool $ i1 > i2, env'')
    _                  -> Left "Gt applied to non-integers"
binary env Eql e1 e2 = do
  (e1', env')  <- eval env e1
  (e2', env'') <- eval env' e2
  case (e1', e2') of
    (EInt i1, EInt i2) -> Right (EBool $ i1 == i2, env'')
    _                  -> Left "Eql applied to non-integers"
binary env Or e1 e2 = do
  (e1', env')  <- eval env e1
  (e2', env'') <- eval env' e2
  case (e1', e2') of
    (EBool b1, EBool b2) -> Right (EBool $ b1 || b2, env'')
    _                    -> Left "Or applied to non-booleans"
binary env And e1 e2 = do
  (e1', env')  <- eval env e1
  (e2', env'') <- eval env' e2
  case (e1', e2') of
    (EBool b1, EBool b2) -> Right (EBool $ b1 && b2, env'')
    _                    -> Left "And applied to non-booleans"
binary env Concat e1 e2 = do
  (e1', env')  <- eval env e1
  (e2', env'') <- eval env' e2
  case (e1', e2') of
    (EStr s1, EStr s2) -> Right (EStr $ BS.append s1 s2, env'')
    _                  -> Left "Concat applied to non-strings"
binary env Take e1 e2 = do
  (e1', env')  <- eval env e1
  (e2', env'') <- eval env' e2
  case (e1', e2') of
    (EInt i, EStr s) -> Right (EStr $ BS.take (fromIntegral i) s, env)
    _                -> Left "Take applied to non-integer and non-string"
binary env Drop e1 e2 = do
  (e1', env')  <- eval env e1
  (e2', env'') <- eval env' e2
  case (e1', e2') of
    (EInt i, EStr s) -> Right (EStr $ BS.drop (fromIntegral i) s, env)
    _                -> Left "Drop applied to non-integer and non-string"
binary env Apply e1 e2 = do
  case e1 of
    ELambda v e1' -> let env' = (v, e2) : env in eval env' e1'
    _           -> Left ("Apply applied to non-lambda: " ++ show e1)


-----
test :: Expr -> String -> Bool
test exp s = do
  let Right e = parseExpr "test" $ BS.pack s
  let Right (actual, _) = eval [] $? e
  actual == exp

-- Unary
_test1 = test (EInt (-3)) "U- I$"
_test2 = test (EBool False) "U! T"
_test3 = test (EInt 15818151) "U# S4%34"
_test4 = test (EStr "test") "U$ I4%34"

-- Binary
_test5 = test (EInt 5) "B+ I# I$"
_test6 = test (EInt 1) "B- I$ I#"
_test7 = test (EInt 6) "B* I$ I#"
_test8 = test (EInt (-3)) "B/ U- I( I#"
_test9 = test (EInt (-1)) "B% U- I( I#"
_test10 = test (EBool False) "B< I$ I#"
_test11 = test (EBool True) "B> I$ I#"
_test12 = test (EBool False) "B= I$ I#"
_test13 = test (EBool True) "B| T F"
_test14 = test (EBool False) "B& T F"
_test15 = test (EStr "test") "B. S4% S34"
_test16 = test (EStr "tes") "BT I$ S4%34"
_test17 = test (EStr "t") "BD I$ S4%34"

-- If
_test18 = test (EStr "no") "? B> I# I$ S9%3 S./"

-- Lambda
_test19 = test (EStr "Hello World!") "B$ B$ L# L$ v# B. SB%,,/ S}Q/2,$_ IK"


-- Evaluation
_test20 = test (EInt 12) "B$ L# B$ L\" B+ v\" v\" B* I$ I# v8"

-- Limit
_test21 = test (EInt 1) "B$ B$ L\" B$ L# B$ v\" B$ v# v# L# B$ v\" B$ v# v# L\" L# ? B= v# I! I\" B$ L$ B+ B$ v\" v$ B$ v\" v$ B- v# I\" I%"

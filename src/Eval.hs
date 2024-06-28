{-# LANGUAGE OverloadedStrings #-}
module Eval where

import Data.Char (ord)
import Data.List (foldl')
import qualified Data.ByteString.Char8 as BS
import Debug.Trace (trace)

import Parser (parseExpr)
import Expr
f $? x = let v = f x
             msg = "{- " ++ show x ++ " => " ++ show v ++ " -}\n\n"
         in trace msg v



type Env = [(Var, Expr)]

eval :: Env -> Expr -> Either String (Env, Expr)
eval env e = case e of
  EBool b -> Right (env, e)
  EInt i  -> Right (env, e)
  EStr s  -> Right (env, e)
  EUnary op e -> unary env op e
  EBinary op e1 e2 -> binary env op e1 e2
  EIf c t e -> do
    (_, c') <- eval env c
    case c' of
      EBool True  -> eval env t
      EBool False -> eval env e
      _           -> Left "If condition is not a boolean"
  ELambda v b -> Right (env, ELambda v b) -- TODO
  EVar v      -> maybe (Left msg) (eval env) $ lookup v env
    where
      msg = "Variable " ++ show v ++ " not found"

unary :: Env -> UOp -> Expr -> Either String (Env, Expr)
unary env Not e = do
  (_, e') <- eval env e
  case e' of
    EBool b -> Right (env, EBool $ not b)
    _       -> Left $ "Not applied to non-boolean: e=" ++ show e'
unary env Neg e = do
  (_, e') <- eval env e
  case e' of
    EInt i -> Right (env, EInt $ negate i)
    _      -> Left $ "Neg applied to non-integer: e=" ++ show e'
unary env StrToInt e = do
  (_, e') <- eval env e
  case e' of
    EStr cs -> Right (env, EInt i)
      where
        cs' = tail $ BS.unpack $ encodeStr cs
        i = fromIntegral $ foldl' (\acc c -> acc * 94 + ord c - ord '!') 0 cs'
    _       -> Left $ "StrToInt applied to non-string: e=" ++ show e'
unary env IntToStr e = do
  (_, e') <- eval env e
  case e' of
    EInt i -> Right (env, EStr $ cultToHuman $ encodeBase94 i)
    _      -> Left $ "IntToStr applied to non-integer: e=" ++ show e'

binary :: Env -> BinOp -> Expr -> Expr -> Either String (Env, Expr)
binary env Add e1 e2 = do
  (_, e1') <- eval env e1
  (_, e2') <- eval env e2
  case (e1', e2') of
    (EInt i1, EInt i2) -> Right (env, EInt $ i1 + i2)
    _                  -> Left $ "Add applied to non-integers: e1=" ++ show e1' ++ ",e2=" ++ show e2'
binary env Sub e1 e2 = do
  (_, e1') <- eval env e1
  (_, e2') <- eval env e2
  case (e1', e2') of
    (EInt i1, EInt i2) -> Right (env, EInt $ i1 - i2)
    _                  -> Left $ "Sub applied to non-integers: e1=" ++ show e1' ++ ",e2=" ++ show e2'
binary env Mult e1 e2 = do
  (_, e1') <- eval env e1
  (_, e2') <- eval env e2
  case (e1', e2') of
    (EInt i1, EInt i2) -> Right (env, EInt $ i1 * i2)
    _                  -> Left $ "Mult applied to non-integers: e1=" ++ show e1' ++ ",e2=" ++ show e2
binary env Div e1 e2 = do
  (_, e1') <- eval env e1
  (_, e2') <- eval env e2
  case (e1', e2') of
    (EInt i1, EInt i2) -> Right (env, EInt $ i1 `quot` i2)
    _                  -> Left $ "Div applied to non-integers: e1=" ++ show e1' ++ ",e2=" ++ show e2'
binary env Mod e1 e2 = do
  (_, e1') <- eval env e1
  (_, e2') <- eval env e2
  case (e1', e2') of
    (EInt i1, EInt i2) -> Right (env, EInt $ i1 `rem` i2)
    _                  -> Left $ "Mod applied to non-integers: e1=" ++ show e1' ++ ",e2=" ++ show e2'
binary env Lt e1 e2 = do
  (_, e1') <- eval env e1
  (_, e2') <- eval env e2
  case (e1', e2') of
    (EInt i1, EInt i2) -> Right (env, EBool $ i1 < i2)
    _                  -> Left $ "Lt applied to non-integers: e1=" ++ show e1' ++ ",e2=" ++ show e2
binary env Gt e1 e2 = do
  (_, e1') <- eval env e1
  (_, e2') <- eval env e2
  case (e1', e2') of
    (EInt i1, EInt i2) -> Right (env, EBool $ i1 > i2)
    _                  -> Left $ "Gt applied to non-integers: e1=" ++ show e1' ++ ",e2=" ++ show e2
binary env Eql e1 e2 = do
  (_, e1') <- eval env e1
  (_, e2') <- eval env e2
  case (e1', e2') of
    (EInt i1, EInt i2) -> Right (env, EBool $ i1 == i2)
    _                  -> Left $ "Eql applied to non-integers: e1=" ++ show e1' ++ ",e2=" ++ show e2'
binary env Or e1 e2 = do
  (_, e1') <- eval env e1
  (_, e2') <- eval env e2
  case (e1', e2') of
    (EBool b1, EBool b2) -> Right (env, EBool $ b1 || b2)
    _                    -> Left $ "Or applied to non-booleans: e1=" ++ show e1' ++ ",e2=" ++ show e2
binary env And e1 e2 = do
  (_, e1') <- eval env e1
  (_, e2') <- eval env e2
  case (e1', e2') of
    (EBool b1, EBool b2) -> Right (env, EBool $ b1 && b2)
    _                    -> Left $ "And applied to non-booleans: e1=" ++ show e1' ++ ",e2=" ++ show e2
binary env Concat e1 e2 = do
  (_, e1') <- eval env e1
  (_, e2') <- eval env e2
  case (e1', e2') of
    (EStr s1, EStr s2) -> Right (env, EStr $ BS.append s1 s2)
    _                  -> Left $ "Concat applied to non-strings: e1=" ++ show e1' ++ ",e2=" ++ show e2
binary env Take e1 e2 = do
  (_, e1') <- eval env e1
  (_, e2') <- eval env e2
  case (e1', e2') of
    (EInt i, EStr s) -> Right (env, EStr $ BS.take (fromIntegral i) s)
    _                -> Left $ "Take applied to non-integer and non-string: e1=" ++ show e1' ++ ",e2=" ++ show e2
binary env Drop e1 e2 = do
  (_, e1') <- eval env e1
  (_, e2') <- eval env e2
  case (e1', e2') of
    (EInt i, EStr s) -> Right (env, EStr $ BS.drop (fromIntegral i) s)
    _                -> Left $ "Drop applied to non-integer and non-string: e1=" ++ show e1' ++ ",e2=" ++ show e2
binary env Apply e1 e2 = do
  case e1 of
    EBinary _ _ _ -> do
      (env', e1') <- eval env e1
      eval env' (EBinary Apply e1' e2)
    EVar v -> do
      (_, e1') <- eval env e1
      eval env (EBinary Apply e1' e2)
    ELambda v e'  -> let env' = (v, e2):env in eval (id $? env') $? e'
    _             -> Left $ "Apply applied to non-lambda: e1=" ++ show e1 ++ ",e2=" ++ show e2 ++ ",env=" ++ show env


-----
test :: Expr -> String -> (Bool, Expr, Expr)
test expected s = do
  let Right e = parseExpr "test" $ BS.pack s
  let Right (_, actual) = eval [] e
  (expected == actual, expected, actual)

-- Unary
_testNeg = test (EInt (-3)) "U- I$"
_testNot = test (EBool False) "U! T"
_testI2S = test (EInt 15818151) "U# S4%34"
_testS2I = test (EStr "test") "U$ I4%34"

-- Binary
_testAdd  = test (EInt 5) "B+ I# I$"
_testSub  = test (EInt 1) "B- I$ I#"
_testMul  = test (EInt 6) "B* I$ I#"
_testQuot = test (EInt (-3)) "B/ U- I( I#"
_testRem  = test (EInt (-1)) "B% U- I( I#"
_testLt   = test (EBool False) "B< I$ I#"
_testGt   = test (EBool True) "B> I$ I#"
_testEq   = test (EBool False) "B= I$ I#"
_testOr   = test (EBool True) "B| T F"
_testAnd  = test (EBool False) "B& T F"
_testComp = test (EStr "test") "B. S4% S34"
_testTake = test (EStr "tes") "BT I$ S4%34"
_testDrop = test (EStr "t") "BD I$ S4%34"

-- If
_testIf = test (EStr "no") "? B> I# I$ S9%3 S./"

-- Lambda
_testLam = test (EStr "Hello World!") "B$ B$ L# L$ v# B. SB%,,/ S}Q/2,$_ IK"


-- Evaluation
_testEval = test (EInt 12) "B$ L# B$ L\" B+ v\" v\" B* I$ I# v8"

-- Limit
_testLim = test (EInt 1) "B$ B$ L\" B$ L# B$ v\" B$ v# v# L# B$ v\" B$ v# v# L\" L# ? B= v# I! I\" B$ L$ B+ B$ v\" v$ B$ v\" v$ B- v# I\" I%"

-- I combinator
-- I 42
_testI = test (EInt 42) "B$ L# v# IK"

-- S combinator
-- S K K 42
_testS = test (EInt 42) "B$ B$ B$ L# L$ L% B$ B$ v# v% B$ v$ v% L# L$ v# L# L$ v# IK"

-- K combinator
-- K 42 3
_testK = test (EInt 42) "B$ B$ L# L$ v# IK I!"

_p21 :: BS.ByteString
_p21 = "B$ B$ L\" B$ L# B$ v\" B$ v# v# L# B$ v\" B$ v# v# L\" L# ? B= v# I! I\" B$ L$ B+ B$ v\" v$ B$ v\" v$ B- v# I\" I%"
{--
_e21' = ((\v1 -> ((\v2 -> v1 (v2 v2)) (\v2 -> v1 (v2 v2))))
         (\v1 -> (\v2 -> if v2 == 0 then 1 else (\v3 -> (v1 v3) + (v1 v3)) (v2 - 1)))
        ) 4
--}
_testAll = and $ map fst3 [ _testNeg,
                            _testNot,
                            _testI2S,
                            _testS2I,
                            _testAdd,
                            _testSub,
                            _testMul,
                            _testQuot,
                            _testRem,
                            _testLt,
                            _testGt,
                            _testEq,
                            _testOr,
                            _testAnd,
                            _testComp,
                            _testTake,
                            _testDrop,
                            _testIf,
                            _testLam,
                            _testEval,
                            _testLim
                          ]
          where fst3 (a, _, _) = a


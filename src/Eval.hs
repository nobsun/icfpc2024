{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Eval where

import Data.Char (ord)
import Data.List (foldl')
import qualified Data.ByteString.Char8 as BS
import qualified Data.IntSet as IntSet
import Debug.Trace (trace)

import Parser (parseExpr)
import Expr

($?) :: (Show p, Show a) => (p -> a) -> p -> a
f $? x = let v = f x
             msg = "{- " ++ show x ++ " => " ++ show v ++ " -}\n\n"
         in trace msg v



type Env = [(Var, Expr)]

type BetaReductionCount = Integer

evalExpr :: Expr -> Either String (Env, BetaReductionCount, Expr)
evalExpr = eval [] 0

eval :: Env -> BetaReductionCount -> Expr -> Either String (Env, BetaReductionCount, Expr)
eval env brc e = case e of
  EBool _b  -> Right (env, brc, e)
  EInt  _i  -> Right (env, brc, e)
  EStr  _s  -> Right (env, brc, e)
  EUnary op _e -> unary env brc op e
  EBinary op e1 e2 -> binary env brc op e1 e2
  EIf c t _e -> do
    (_, brc', c') <- eval env brc c
    case c' of
      EBool True  -> eval env brc' t
      EBool False -> eval env brc' e
      _           -> Left "If condition is not a boolean"
  ELambda v b -> Right (env, brc, ELambda v b) -- TODO
  EVar v      -> maybe (Left msg) (eval env brc) $ lookup v env
    where
      msg = "Variable " ++ show v ++ " not found"

unary :: Env -> BetaReductionCount -> UOp -> Expr -> Either String (Env, BetaReductionCount, Expr)
unary env brc Not e = do
  (_, brc', e') <- eval env brc e
  case e' of
    EBool b -> Right (env, brc', EBool $ not b)
    _       -> Left $ "Not applied to non-boolean: e=" ++ show e'
unary env brc Neg e = do
  (_, brc', e') <- eval env brc e
  case e' of
    EInt i -> Right (env, brc', EInt $ negate i)
    _      -> Left $ "Neg applied to non-integer: e=" ++ show e'
unary env brc StrToInt e = do
  (_, brc', e') <- eval env brc e
  case e' of
    EStr cs -> Right (env, brc', EInt i)
      where
        cs' = tail $ BS.unpack $ encodeStr cs
        i = fromIntegral $ foldl' (\acc c -> acc * 94 + ord c - ord '!') 0 cs'
    _       -> Left $ "StrToInt applied to non-string: e=" ++ show e'
unary env brc IntToStr e = do
  (_, brc', e') <- eval env brc e
  case e' of
    EInt i -> Right (env, brc', EStr $ cultToHuman $ encodeBase94 i)
    _      -> Left $ "IntToStr applied to non-integer: e=" ++ show e'

binary :: Env -> BetaReductionCount -> BinOp -> Expr -> Expr -> Either String (Env, BetaReductionCount, Expr)
binary env brc Add e1 e2 = do
  (_, brc',  e1') <- eval env brc e1
  (_, brc'', e2') <- eval env brc' e2
  case (e1', e2') of
    (EInt i1, EInt i2) -> Right (env, brc'', EInt $ i1 + i2)
    _                  -> Left $ "Add applied to non-integers: e1=" ++ show e1' ++ ",e2=" ++ show e2'
binary env brc Sub e1 e2 = do
  (_, brc',  e1') <- eval env brc e1
  (_, brc'', e2') <- eval env brc' e2
  case (e1', e2') of
    (EInt i1, EInt i2) -> Right (env, brc'', EInt $ i1 - i2)
    _                  -> Left $ "Sub applied to non-integers: e1=" ++ show e1' ++ ",e2=" ++ show e2'
binary env brc Mult e1 e2 = do
  (_, _brc', e1') <- eval env brc e1
  (_, brc'', e2') <- eval env brc e2
  case (e1', e2') of
    (EInt i1, EInt i2) -> Right (env, brc'', EInt $ i1 * i2)
    _                  -> Left $ "Mult applied to non-integers: e1=" ++ show e1' ++ ",e2=" ++ show e2
binary env brc Div e1 e2 = do
  (_, brc',  e1') <- eval env brc e1
  (_, brc'', e2') <- eval env brc' e2
  case (e1', e2') of
    (EInt i1, EInt i2) -> Right (env, brc'', EInt $ i1 `quot` i2)
    _                  -> Left $ "Div applied to non-integers: e1=" ++ show e1' ++ ",e2=" ++ show e2'
binary env brc Mod e1 e2 = do
  (_, brc',  e1') <- eval env brc e1
  (_, brc'', e2') <- eval env brc' e2
  case (e1', e2') of
    (EInt i1, EInt i2) -> Right (env, brc'', EInt $ i1 `rem` i2)
    _                  -> Left $ "Mod applied to non-integers: e1=" ++ show e1' ++ ",e2=" ++ show e2'
binary env brc Lt e1 e2 = do
  (_, brc',  e1') <- eval env brc e1
  (_, brc'', e2') <- eval env brc' e2
  case (e1', e2') of
    (EInt i1, EInt i2) -> Right (env, brc'', EBool $ i1 < i2)
    _                  -> Left $ "Lt applied to non-integers: e1=" ++ show e1' ++ ",e2=" ++ show e2
binary env brc Gt e1 e2 = do
  (_, brc',  e1') <- eval env brc e1
  (_, brc'', e2') <- eval env brc' e2
  case (e1', e2') of
    (EInt i1, EInt i2) -> Right (env, brc'', EBool $ i1 > i2)
    _                  -> Left $ "Gt applied to non-integers: e1=" ++ show e1' ++ ",e2=" ++ show e2
binary env brc Eql e1 e2 = do
  (_, brc',  e1') <- eval env brc e1
  (_, brc'', e2') <- eval env brc' e2
  case (e1', e2') of
    (EBool b1, EBool b2) -> Right (env, brc'', EBool $ b1 == b2)
    (EInt i1,   EInt i2) -> Right (env, brc'', EBool $ i1 == i2)
    (EStr s1,   EStr s2) -> Right (env, brc'', EBool $ s1 == s2)
    _                  -> Left $ "Eql applied to non-integers: e1=" ++ show e1' ++ ",e2=" ++ show e2'
binary env brc Or e1 e2 = do
  (_, brc',  e1') <- eval env brc e1
  (_, brc'', e2') <- eval env brc' e2
  case (e1', e2') of
    (EBool b1, EBool b2) -> Right (env, brc'', EBool $ b1 || b2)
    _                    -> Left $ "Or applied to non-booleans: e1=" ++ show e1' ++ ",e2=" ++ show e2
binary env brc And e1 e2 = do
  (_, brc',  e1') <- eval env brc e1
  (_, brc'', e2') <- eval env brc' e2
  case (e1', e2') of
    (EBool b1, EBool b2) -> Right (env, brc'', EBool $ b1 && b2)
    _                    -> Left $ "And applied to non-booleans: e1=" ++ show e1' ++ ",e2=" ++ show e2
binary env brc Concat e1 e2 = do
  (_, brc',  e1') <- eval env brc e1
  (_, brc'', e2') <- eval env brc' e2
  case (e1', e2') of
    (EStr s1, EStr s2) -> Right (env, brc'', EStr $ BS.append s1 s2)
    _                  -> Left $ "Concat applied to non-strings: e1=" ++ show e1' ++ ",e2=" ++ show e2
binary env brc Take e1 e2 = do
  (_, brc',  e1') <- eval env brc e1
  (_, brc'', e2') <- eval env brc' e2
  case (e1', e2') of
    (EInt i, EStr s) -> Right (env, brc'', EStr $ BS.take (fromIntegral i) s)
    _                -> Left $ "Take applied to non-integer and non-string: e1=" ++ show e1' ++ ",e2=" ++ show e2
binary env brc Drop e1 e2 = do
  (_, brc',  e1') <- eval env brc e1
  (_, brc'', e2') <- eval env brc' e2
  case (e1', e2') of
    (EInt i, EStr s) -> Right (env, brc'', EStr $ BS.drop (fromIntegral i) s)
    _                -> Left $ "Drop applied to non-integer and non-string: e1=" ++ show e1' ++ ",e2=" ++ show e2
binary env brc Apply e1 e2 = do
  (env', brc', e1') <- eval env brc e1
  case renameBoundVariables (IntSet.fromList $ map fst env') e1' of
    ELambda v e'  -> let env'' = (v, e2):env' in eval env'' (brc'+1) e'
    _             -> Left $ "Apply applied to non-lambda: e1=" ++ show e1 ++ ",e2=" ++ show e2 ++ ",env=" ++ show env


-----
type TestResult = (Bool, BetaReductionCount, Expr, Expr)

test :: Expr -> String -> TestResult
test expected s = do
  let Right e = parseExpr "test" $ BS.pack s
  let Right (_, bcr, actual) = eval [] 0 e
  (expected == actual, bcr, expected, actual)

-- Unary
_testNeg = test (EInt (-3)) "U- I$"
_testNot = test (EBool False) "U! T"
_testI2S = test (EInt 15818151) "U# S4%34"
_testS2I = test (EStr "test") "U$ I4%34"

_testNeg, _testNot, _testI2S, _testS2I :: TestResult

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


_testAdd, _testSub, _testMul, _testQuot, _testRem  :: TestResult
_testLt, _testGt, _testEq, _testOr, _testAnd, _testComp, _testTake, _testDrop :: TestResult

-- If
_testIf = test (EStr "no") "? B> I# I$ S9%3 S./"

-- Lambda
_testLam = test (EStr "Hello World!") "B$ B$ L# L$ v# B. SB%,,/ S}Q/2,$_ IK"


-- Evaluation
_testEval = test (EInt 12) "B$ L# B$ L\" B+ v\" v\" B* I$ I# v8"

-- Limit
_testLim = test (EInt 16) "B$ B$ L\" B$ L# B$ v\" B$ v# v# L# B$ v\" B$ v# v# L\" L# ? B= v# I! I\" B$ L$ B+ B$ v\" v$ B$ v\" v$ B- v# I\" I%"
_testLim' :: Bool
_testLim' = snd4 _testLim == 109
  where snd4 (_a, b, _c, _d) = b

-- I combinator
-- I 42
_testI = test (EInt 42) "B$ L# v# IK"

-- S combinator
-- S K K 42
_testS = test (EInt 42) "B$ B$ B$ L# L$ L% B$ B$ v# v% B$ v$ v% L# L$ v# L# L$ v# IK"

-- K combinator
-- K 42 3
_testK = test (EInt 42) "B$ B$ L# L$ v# IK I!"

_testIf, _testLam, _testEval, _testLim, _testI, _testS, _testK :: TestResult
--   ,_testAll

_p21 :: BS.ByteString
_p21 = "B$ B$ L\" B$ L# B$ v\" B$ v# v# L# B$ v\" B$ v# v# L\" L# ? B= v# I! I\" B$ L$ B+ B$ v\" v$ B$ v\" v$ B- v# I\" I%"

_p21' :: BS.ByteString
_p21' = "[(L\" [(L# (v\" (v# v#))) (L# (v\" (v# v#)))])    [L\" L# {? (B= v# I!) I\" [(L$ (B+ (v\" v$) (v\" v$))) (B- v# I\")]}]]   I%"

_testAll :: Bool
_testAll = and $ map fst4 _allTests
          where fst4 (a, _, _, _) = a
_allTests :: [TestResult]
_allTests = [ _testNeg,
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

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Interp
    ( Value (..)
    , Env
    , interp
    ) where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe
import Expr
import Parser
import Debug.Trace
import System.IO.Unsafe

type Env = M.Map Var (State Int Value)

data Value
    = VBottom String
    | VBool Bool
    | VInt  Integer
    | VStr  ByteString
    | VFun  (State Int Value -> State Int Value)

instance Show Value where
    showsPrec _ = \ case
        VBottom msg -> (('⊥' :) . (msg ++))
        VBool b -> shows b . (" :: Bool" ++)
        VInt  n -> shows n . (" :: Int"  ++)
        VStr  s -> (BS.unpack s ++) . (" :: String" ++)
        VFun  _ -> ("[λ x . e]" ++) . (" :: Function" ++)
   
interp :: Env -> Expr -> State Int Value
interp rho = \ case
    EBool b     -> pure (VBool b)
    EInt  n     -> pure (VInt n)
    EStr str    -> pure (VStr str)
    EUnary op e -> interp rho e >>= exUOp op
    EBinary Apply e1 e2 -> interp rho e1 >>= \ case
        VFun f -> modify succ >> f (interp rho e2)
        _      -> pure $ VBottom "not function"
    EBinary op e1 e2 -> interp rho e1 >>= \ v1 ->
                        interp rho e2 >>= \ v2 ->
                        case (v1, v2) of
        (VInt m, VInt n) -> case op of
            Add  -> pure (VInt (m+n))
            Sub  -> pure (VInt (m - n))
            Mult -> pure (VInt (m * n))
            Div  -> pure (VInt (m `quot` n))
            Mod  -> pure (VInt (m `rem` n))
            Lt   -> pure (VBool (m < n))
            Gt   -> pure (VBool (m > n))
            Eql  -> pure (VBool (m == n))
            _    -> pure (VBottom "operands are not Ints")
        (VBool p, VBool q) -> case op of
            Or   -> pure (VBool (p || q))
            And  -> pure (VBool (p && q))
            Eql  -> pure (VBool (p == q))
            _    -> pure (VBottom "operands are not Bools")
        (VStr s, VStr t) -> case op of
            Concat -> pure (VStr (s <> t))
            Eql    -> pure (VBool (s == t))
            _      -> pure (VBottom "operands are Strings but the operator ..")
        (VInt n, VStr s) -> case op of
            Take -> pure (VStr $ BS.take (fromInteger n) s)
            Drop -> pure (VStr $ BS.drop (fromInteger n) s)
            _    -> pure (VBottom "operator must be take or drop")
        _ -> pure (VBottom "unknow error")
    EIf e e1 e2 -> interp rho e >>= \ case
        VBool b | b         -> interp rho e1
                | otherwise -> interp rho e2
        _ -> pure (VBottom "condition is not boolean")
    EVar v -> fromMaybe (pure (VBottom "unbounded variable")) $ M.lookup v rho
    ELambda v e -> pure 
        $ VFun (\ a -> interp (M.insert v a rho) e)

exUOp :: UOp -> Value -> State Int Value
exUOp uop val = case uop of
    Neg -> case val of
        VInt n -> pure (VInt (negate n))
        _      -> pure (VBottom "operand is not Int")
    Not -> case val of
        VBool b -> pure (VBool (not b))
        _       -> pure (VBottom "operand is not Bool")
    StrToInt -> case val of
        VStr str -> pure (VInt (decodeBase94 (humanToCult str)))
        _        -> pure $ VBottom "operand is not String"
    IntToStr -> case val of
        VInt n -> pure (VStr (cultToHuman (encodeBase94 n)))
        _      -> pure $ VBottom "operand is not Int"

_languageTestExpr :: Expr
_languageTestExpr = EIf (EBinary Eql (EBinary Apply (EBinary Apply (EBinary Apply (EBinary Apply (ELambda 3 (ELambda 3 (ELambda 3 (ELambda 2 (EVar 3))))) (EInt 1)) (EInt 2)) (EInt 3)) (EInt 4)) (EInt 3)) (EIf (EBinary Eql (EBinary Apply (ELambda 3 (EVar 3)) (EInt 10)) (EInt 10)) (EIf (EBinary Eql (EBinary Drop (EInt 3) (EStr "test")) (EStr "t")) (EIf (EBinary Eql (EBinary Take (EInt 3) (EStr "test")) (EStr "tes")) (EIf (EBinary Eql (EBinary Concat (EStr "te") (EStr "st")) (EStr "test")) (EIf (EUnary Not (EBinary And (EBool True) (EBool False))) (EIf (EBinary And (EBool True) (EBool True)) (EIf (EUnary Not (EBinary Or (EBool False) (EBool False))) (EIf (EBinary Or (EBool False) (EBool True)) (EIf (EBinary Lt (EUnary Neg (EInt 3)) (EUnary Neg (EInt 2))) (EIf (EBinary Gt (EInt 3) (EInt 2)) (EIf (EBinary Eql (EUnary Neg (EInt 1)) (EBinary Mod (EUnary Neg (EInt 3)) (EInt 2))) (EIf (EBinary Eql (EInt 1) (EBinary Mod (EInt 7) (EInt 3))) (EIf (EBinary Eql (EUnary Neg (EInt 1)) (EBinary Div (EUnary Neg (EInt 3)) (EInt 2))) (EIf (EBinary Eql (EInt 2) (EBinary Div (EInt 7) (EInt 3))) (EIf (EBinary Eql (EInt 6) (EBinary Mult (EInt 2) (EInt 3))) (EIf (EBinary Eql (EInt 3) (EBinary Add (EInt 1) (EInt 2))) (EIf (EBinary Eql (EUnary IntToStr (EInt 15818151)) (EStr "test")) (EIf (EBinary Eql (EUnary StrToInt (EStr "test")) (EInt 15818151)) (EIf (EUnary Not (EBool False)) (EIf (EBinary Eql (EUnary Neg (EInt 3)) (EBinary Sub (EInt 2) (EInt 5))) (EIf (EBinary Eql (EInt 3) (EBinary Sub (EInt 5) (EInt 2))) (EIf (EBinary Eql (EStr "test") (EStr "test")) (EIf (EBinary Eql (EBool False) (EBool False)) (EIf (EBinary Eql (EInt 3) (EInt 3)) (EIf (EBool True) (EBinary Concat (EBinary Concat (EStr "Self-check OK, send `solve language_test ") (EUnary IntToStr (EBinary Add (EInt 2) (EBinary Mult (EInt 311) (EInt 124753942619))))) (EStr "` to claim points for it")) (EStr "if is not correct")) (EStr "binary = is not correct")) (EStr "binary = is not correct")) (EStr "binary = is not correct")) (EStr "binary - is not correct")) (EStr "unary - is not correct")) (EStr "unary ! is not correct")) (EStr "unary # is not correct")) (EStr "unary $ is not correct")) (EStr "binary + is not correct")) (EStr "binary * is not correct")) (EStr "binary / is not correct")) (EStr "binary / is not correct")) (EStr "binary % is not correct")) (EStr "binary % is not correct")) (EStr "binary > is not correct")) (EStr "binary < is not correct")) (EStr "binary | is not correct")) (EStr "binary | is not correct")) (EStr "binary & is not correct")) (EStr "binary & is not correct")) (EStr "binary . is not correct")) (EStr "binary T is not correct")) (EStr "binary D is not correct")) (EStr "application is not correct")) (EStr "application is not correct")

_parse :: ByteString -> Expr
_parse s = case parseExpr "test" $ s of
    Right e -> e
    _       -> error "invalid string"

_interp :: ByteString -> (Value, Int)
_interp s = runState (interp M.empty (_parse s)) 0

_limit :: ByteString
_limit = "B$ B$ L\" B$ L# B$ v\" B$ v# v# L# B$ v\" B$ v# v# L\" L# ? B= v# I! I\" B$ L$ B+ B$ v\" v$ B$ v\" v$ B- v# I\" I%"

{- --
test :: Expr -> String -> (Bool, Expr, Expr)
test expected s = do
  let Right e = parseExpr "test" $ BS.pack s
  let Right (_, actual) = eval [] e
  (expected == actual, expected, actual)
-- --
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
-- -}

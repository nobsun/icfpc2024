{-# LANGUAGE LambdaCase #-}
module Interp
    ( Value (..)
    , Env
    , interp
    ) where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS (take, drop)
import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe
import Expr

type Env = M.Map Var (State Int Value)

data Value
    = VBottom
    | VBool Bool
    | VInt  Integer
    | VStr  ByteString
    | VFun  (State Int Value -> State Int Value)
   
interp :: Env -> Expr -> State Int Value
interp rho = \ case
    EBool b     -> pure (VBool b)
    EInt  n     -> pure (VInt n)
    EStr str    -> pure (VStr str)
    EUnary op e -> interp rho e >>= exUOp op
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
            _    -> pure VBottom
        (VBool p, VBool q) -> case op of
            Or   -> pure (VBool (p || q))
            And  -> pure (VBool (p && q))
            Eql  -> pure (VBool (p == q))
            _    -> pure VBottom
        (VStr s, VStr t) -> case op of
            Concat -> pure (VStr (s <> t))
            _      -> pure VBottom
        (VInt n, VStr s) -> case op of
            Take -> pure (VStr $ BS.take (fromInteger n) s)
            Drop -> pure (VStr $ BS.drop (fromInteger n) s)
            _    -> pure VBottom
        (VFun f, v) -> modify succ >> f (pure v)
        _ -> pure VBottom
    EIf e e1 e2 -> interp rho e >>= \ case
        VBool b | b         -> interp rho e1
                | otherwise -> interp rho e2
        _ -> pure VBottom
    EVar v -> fromMaybe (pure VBottom) $ M.lookup v rho
    ELambda v e -> pure 
        $ VFun (\ a -> interp (M.insert v a rho) e)

exUOp :: UOp -> Value -> State Int Value
exUOp uop val = case uop of
    Neg -> case val of
        VInt n -> pure (VInt (negate n))
        _      -> pure VBottom
    Not -> case val of
        VBool b -> pure (VBool (not b))
        _       -> pure VBottom
    StrToInt -> case val of
        VStr str -> pure (VInt (decodeBase94 str))
        _        -> pure VBottom
    IntToStr -> case val of
        VInt n -> pure (VStr (encodeBase94 n))
        _      -> pure VBottom

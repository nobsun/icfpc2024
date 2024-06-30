module Value
  ( Value (..)

  , vNeg
  , vNot
  , vStrToInt
  , vIntToStr

  , vAdd
  , vSub
  , vMult
  , vDiv
  , vMod
  , vLt
  , vGt
  , vEql
  , vOr
  , vAnd
  , vConcat
  , vTake
  , vDrop
  , vApply

  , vIf

  , exprToHaskellExpr
  , eval
  ) where


import qualified Data.ByteString.Char8 as BS
import qualified Data.IntMap.Lazy as IntMap
import Expr

data Value
  = VBool !Bool
  | VInt !Integer
  | VStr BS.ByteString
  | VFun (Value -> Value)

instance Show Value where
  show v = case v of
    VBool b  -> "VBool " ++ show b
    VInt  i  -> "VInt "  ++ show i
    VStr  s  -> "VStr "  ++ show s
    VFun  _  -> "VFun {..}"

verror :: Show v => String -> String -> [v] -> a
verror n t vs = error $ n ++ ": not " ++ t ++ ": " ++ unwords (map show vs)

vNeg :: Value -> Value
vNeg (VInt x) = VInt (-x)
vNeg  v       = verror "vNeg" "int" [v]

vNot :: Value -> Value
vNot (VBool x) = VBool $ not x
vNot  v        = verror "vNot" "bool" [v]

vStrToInt :: Value -> Value
vStrToInt (VStr s) = VInt $ decodeBase94 $ humanToCult s
vStrToInt  v       = verror "vStrToInt" "str" [v]

vIntToStr :: Value -> Value
vIntToStr (VInt n) = VStr $ cultToHuman $ encodeBase94 n
vIntToStr  v       = verror "vIntToStr" "int" [v]

vAdd, vSub, vMult, vDiv, vMod, vLt, vGt, vEql, vOr, vAnd, vConcat, vTake, vDrop,
  vApply, vApplyLazy, vApplyEager :: Value -> Value -> Value
vAdd    (VInt n1) (VInt n2) = VInt (n1 + n2)
vAdd     v1        v2       = verror "vAdd" "int int" [v1, v2]
vSub    (VInt n1) (VInt n2) = VInt (n1 - n2)
vSub     v1        v2       = verror "vSub" "int int" [v1, v2]
vMult   (VInt n1) (VInt n2) = VInt (n1 * n2)
vMult     v1        v2       = verror "vMult" "int int" [v1, v2]
vDiv    (VInt n1) (VInt n2) = VInt (n1 `quot` n2)
vDiv     v1        v2       = verror "vDiv" "int int" [v1, v2]
vMod    (VInt n1) (VInt n2) = VInt (n1 `rem` n2)
vMod     v1        v2       = verror "vMod" "int int" [v1, v2]
vLt     (VInt n1) (VInt n2) = VBool (n1 < n2)
vLt      v1        v2       = verror "vLt" "int int" [v1, v2]
vGt     (VInt n1) (VInt n2) = VBool (n1 > n2)
vGt      v1        v2       = verror "vGt" "int int" [v1, v2]
vEql    (VInt n1) (VInt n2) = VBool (n1 == n2)
vEql    (VBool b1) (VBool b2) = VBool (b1 == b2)
vEql    (VStr s1) (VStr s2) = VBool (s1 == s2)
vEql     v1        v2       = verror "vEql" "int int / bool bool / str str" [v1, v2]
vOr     (VBool b1) (VBool b2) = VBool (b1 || b2)
vOr      v1        v2       = verror "vOr" "bool bool" [v1, v2]
vAnd    (VBool b1) (VBool b2) = VBool (b1 && b2)
vAnd     v1        v2       = verror "vAnd" "bool bool" [v1, v2]
vConcat (VStr s1) (VStr s2) = VStr (s1 <> s2)
vConcat  v1        v2       = verror "vConcat" "str str" [v1, v2]
vTake   (VInt n) (VStr s) = VStr (BS.take (fromIntegral n) s)
vTake    v1        v2     = verror "vTake" "int int" [v1, v2]
vDrop   (VInt n) (VStr s) = VStr (BS.drop (fromIntegral n) s)
vDrop    v1        v2     = verror "vTake" "int int" [v1, v2]
vApply  (VFun f) v = f v
vApply   f       v = verror "vApply" "fun value" [f, v]
vApplyLazy (VFun f) v = f v
vApplyLazy  f       v = verror "vApplyLazy" "fun value" [f, v]
vApplyEager (VFun f) v = v `seq` f v
vApplyEager  f       v = verror "vApplyEager" "fun value" [f, v]

vIf :: Value -> Value -> Value -> Value
vIf (VBool b) v1 v2 = if b then v1 else v2
vIf  b        v1 v2 = verror "vIf" "bool value value" [b, v1, v2]

exprToHaskellExpr :: Expr -> String
exprToHaskellExpr = ($ "") . f
  where
    f (EBool b) = showParen True $ showString "VBool " . shows b
    f (EInt n) = showParen True $ showString "VInt " . shows n
    f (EStr s) = showParen True $ showString "VStr " . shows s
    f (EUnary op e) = showParen True $ showString ("v" ++ show op ++ " ") . f e
    f (EBinary op e1 e2) = showParen True $ showString ("v" ++ show op ++ " ") . f e1 . showString " " . f e2
    f (EIf e1 e2 e3) = showParen True $ showString "vIf " . f e1 . showString " " . f e2 . showString " " . f e3
    f (ELambda x e) = showParen True $ showString "VFun " . (showParen True $ showString ("\\x"  ++ show x) . showString " -> " . f e)
    f (EVar x) = showString ("x" ++ show x)

eval :: Expr -> Value
eval = f IntMap.empty
  where
    f _env (EBool b) = VBool b
    f _env (EInt n) = VInt n
    f _env (EStr s) = VStr s
    f env (EUnary op e) = op' (f env e)
      where
        op' = case op of
                Neg      -> vNeg
                Not      -> vNot
                StrToInt -> vStrToInt
                IntToStr -> vIntToStr
    f env (EBinary op e1 e2) = op' (f env e1) (f env e2)
      where
        op' = case op of
                Add     -> vAdd
                Sub     -> vSub
                Mult    -> vMult
                Div     -> vDiv
                Mod     -> vMod
                Lt      -> vLt
                Gt      -> vGt
                Eql     -> vEql
                Or      -> vOr
                And     -> vAnd
                Concat  -> vConcat
                Take    -> vTake
                Drop    -> vDrop
                Apply   -> vApply
                ApplyLazy   -> vApplyLazy
                ApplyEager  -> vApplyEager
    f env (EIf e1 e2 e3) = vIf (f env e1) (f env e2) (f env e3)
    f env (ELambda n e) = VFun (\x -> f (IntMap.insert n x env) e)
    f env (EVar n) = env IntMap.! n

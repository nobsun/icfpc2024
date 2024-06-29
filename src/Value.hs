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
  ) where


import qualified Data.ByteString.Char8 as BS
import Expr

data Value
  = VBool !Bool
  | VInt !Integer
  | VStr BS.ByteString
  | VFun (Value -> Value)

vNeg :: Value -> Value
vNeg (VInt x) = VInt (-x)

vNot :: Value -> Value
vNot (VBool x) = VBool $ not x

vStrToInt :: Value -> Value
vStrToInt (VStr s) = VInt $ decodeBase94 $ humanToCult s

vIntToStr :: Value -> Value
vIntToStr (VInt n) = VStr $ cultToHuman $ encodeBase94 n

vAdd, vSub, vMult, vDiv, vMod, vLt, vGt, vEql, vOr, vAnd, vConcat, vTake, vDrop, vApply :: Value -> Value -> Value
vAdd    (VInt n1) (VInt n2) = VInt (n1 + n2)
vSub    (VInt n1) (VInt n2) = VInt (n1 - n2)
vMult   (VInt n1) (VInt n2) = VInt (n1 * n2)
vDiv    (VInt n1) (VInt n2) = VInt (n1 `quot` n2)
vMod    (VInt n1) (VInt n2) = VInt (n1 `rem` n2)
vLt     (VInt n1) (VInt n2) = VBool (n1 < n2)
vGt     (VInt n1) (VInt n2) = VBool (n1 > n2)
vEql    (VInt n1) (VInt n2) = VBool (n1 == n2)
vEql    (VBool b1) (VBool b2) = VBool (b1 == b2)
vEql    (VStr s1) (VStr s2) = VBool (s1 == s2)
vOr     (VBool b1) (VBool b2) = VBool (b1 || b2)
vAnd    (VBool b1) (VBool b2) = VBool (b1 && b2)
vConcat (VStr s1) (VStr s2) = VStr (s1 <> s2)
vTake   (VInt n) (VStr s) = VStr (BS.take (fromIntegral n) s)
vDrop   (VInt n) (VStr s) = VStr (BS.drop (fromIntegral n) s)
vApply  (VFun f) v = f v

vIf :: Value -> Value -> Value -> Value
vIf (VBool b) v1 v2 = if b then v1 else v2

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

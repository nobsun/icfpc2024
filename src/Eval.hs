module Eval where

import Data.Char (ord)
import Data.List (foldl')
import qualified Data.ByteString.Char8 as BS

import Expr

eval :: Expr -> Either String Expr
eval e = case e of
  EBool b -> Right e
  EInt i  -> Right e
  EStr s  -> Right e
  EUnary op e -> do
    e' <- eval e
    unary op e'
  EBinary op e1 e2 -> do
    e1' <- eval e1
    e2' <- eval e2
    binary op e1' e2'

unary :: UOp -> Expr -> Either String Expr
unary Not e = case e of
    EBool b -> Right $ EBool $ not b
    _       -> Left "Not applied to non-boolean"
unary Neg e = case e of
    EInt i -> Right $ EInt $ negate i
    _      -> Left "Neg applied to non-integer"
unary StrToInt e = case e of
    EStr cs -> Right $ EInt i
      where
        cs' = tail $ BS.unpack $ encodeStr cs
        i = fromIntegral $ foldl' (\acc c -> acc * 94 + ord c - ord '!') 0 cs'
    _       -> Left "StrToInt applied to non-string"
unary IntToStr e = case e of
    EInt i -> Right $ EStr $ cultToHuman $ encodeBase94 i
    _      -> Left "IntToStr applied to non-integer"

binary :: BinOp -> Expr -> Expr -> Either String Expr
binary Add e1 e2 = case (e1, e2) of
    (EInt i1, EInt i2) -> Right $ EInt $ i1 + i2
    _                  -> Left "Add applied to non-integers"
binary Sub e1 e2 = case (e1, e2) of
    (EInt i1, EInt i2) -> Right $ EInt $ i1 - i2
    _                  -> Left "Sub applied to non-integers"
binary Mult e1 e2 = case (e1, e2) of
    (EInt i1, EInt i2) -> Right $ EInt $ i1 * i2
    _                  -> Left "Mult applied to non-integers"
binary Div e1 e2 = case (e1, e2) of
    (EInt i1, EInt i2) -> Right $ EInt $ i1 `quot` i2
    _                  -> Left "Div applied to non-integers"
binary Mod e1 e2 = case (e1, e2) of
    (EInt i1, EInt i2) -> Right $ EInt $ i1 `rem` i2
    _                  -> Left "Mod applied to non-integers"
binary Lt e1 e2 = case (e1, e2) of
    (EInt i1, EInt i2) -> Right $ EBool $ i1 < i2
    _                  -> Left "Lt applied to non-integers"
binary Gt e1 e2 = case (e1, e2) of
    (EInt i1, EInt i2) -> Right $ EBool $ i1 > i2
    _                  -> Left "Gt applied to non-integers"
binary Eql e1 e2 = case (e1, e2) of
    (EInt i1, EInt i2) -> Right $ EBool $ i1 == i2
    _                  -> Left "Eql applied to non-integers"
binary Or e1 e2 = case (e1, e2) of
    (EBool b1, EBool b2) -> Right $ EBool $ b1 || b2
    _                    -> Left "Or applied to non-booleans"
binary And e1 e2 = case (e1, e2) of
    (EBool b1, EBool b2) -> Right $ EBool $ b1 && b2
    _                    -> Left "And applied to non-booleans"
binary Concat e1 e2 = case (e1, e2) of
    (EStr s1, EStr s2) -> Right $ EStr $ BS.append s1 s2
    _                  -> Left "Concat applied to non-strings"
binary Take e1 e2 = case (e1, e2) of
    (EInt i, EStr s) -> Right $ EStr $ BS.take (fromIntegral i) s
    _                -> Left "Take applied to non-integer and non-string"
binary Drop e1 e2 = case (e1, e2) of
    (EInt i, EStr s) -> Right $ EStr $ BS.drop (fromIntegral i) s
    _                -> Left "Drop applied to non-integer and non-string"
binary Apply e1 e2 = case e1 of
    ELambda v e -> Right $ e
    _           -> Left "Apply applied to non-lambda"


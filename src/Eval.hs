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
  EUnary op e -> unary op e

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

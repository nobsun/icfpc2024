{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Pretty where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8

import Imports hiding (And)
import Expr

newtype PPString = PPS (Endo String) deriving (Semigroup, Monoid)

pps :: String -> PPString
pps = PPS . Endo . (++)

instance Show PPString where
  show (PPS e) = show $ appEndo e []

instance IsString PPString where
  fromString = pps

showpp :: Show a => a -> PPString
showpp = pps . show

-----

data PprFix
  = PInfix
  | PPrefix

pprExpr :: PprFix -> Expr -> PPString
pprExpr pf e0 = case e0 of
  EBool b            -> pprBool b
  EInt i             -> pprNat i
  EStr s             -> pprStr s
  EUnary op e        -> pprUnary pf op e
  EBinary op e1 e2   -> pprBinary pf op e1 e2
  EIf e1 e2 e3       -> pprIf pf e1 e2 e3
  ELambda v e        -> pprLambda pf v e
  EVar v             -> pprVar v

pprBool :: Bool -> PPString
pprBool True  = "true"
pprBool False = "false"

pprNat :: Integer -> PPString
pprNat = showpp

pprStr :: ByteString -> PPString
pprStr = dquote . pps . B8.unpack

pprUnary :: PprFix -> UOp -> Expr -> PPString
pprUnary pf u e = pprUOp u <+> parenExpr pf e

pprUOp :: UOp -> PPString
pprUOp u = case u of
  Neg       -> "-"
  Not       -> "!"
  StrToInt  -> "str-to-int"
  IntToStr  -> "int-to-str"

pprBinary :: PprFix -> BinOp -> Expr -> Expr -> PPString
pprBinary pf op e1 e2 = pprBOp pf op (parenExpr pf e1) (parenExpr pf e2)

pprBOp :: PprFix -> BinOp -> PPString -> PPString -> PPString
pprBOp pprFix b = case b of
  Add     ->  opInfix "+"
  Sub     ->  opInfix "-"
  Mult    ->  opInfix "*"
  -- Quot    ->  opInfix "/"
  Div     ->  opInfix "/"
  -- Rem     ->  "%"
  Mod     ->  opInfix "%"
  Lt      ->  opInfix "<"
  Gt      ->  opInfix ">"
  Eql     ->  opInfix "="
  Or      ->  opInfix "|"
  And     ->  opInfix "&"
  Concat  ->  opInfix "."
  Take    ->  opPrefix "take"
  Drop    ->  opPrefix "drop"
  Apply   ->  apply
  where
    opInfix  op = case pprFix of
      PInfix   -> pprInfix op
      PPrefix  -> pprPrefix op
    opPrefix op = pprPrefix op
    apply x y = x <+> y

    pprInfix   op x y = x <+> op <+> y
    pprPrefix  op x y = op <+> x <+> y

pprIf :: PprFix -> Expr -> Expr -> Expr -> PPString
pprIf pf e1 e2 e3 = "if" <+> parenExpr pf e1 <+> parenExpr pf e2 <+> parenExpr pf e3

pprLambda :: PprFix -> Var -> Expr -> PPString
pprLambda pf v e = "λ" <+> pprVar v <+> "->" <+> pprExpr pf e

pprVar :: Var -> PPString
pprVar v = "v" <> showpp v

-----

parenExpr :: PprFix -> Expr -> PPString
parenExpr pf e
  | literal e  = pprExpr pf e
  | otherwise  = paren (pprExpr pf e)

literal :: Expr -> Bool
literal e = case e of
  EBool {}  -> True
  EInt  {}  -> True
  EStr  {}  -> True
  EVar  {}  -> True
  _         -> False

paren :: PPString -> PPString
paren s = "(" <> s <> ")"

dquote :: PPString -> PPString
dquote s = "\"" <> s <> "\""

(<+>) :: PPString -> PPString -> PPString
s1 <+> s2 = s1 <> " " <> s2

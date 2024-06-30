{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GHC2021 #-}
module Expr
  ( Expr' (..)
  , Expr
  , Var
  , UOp (..)
  , BinOp (..)
  , Token
  , unELambdaVars
  , toELambdaVars
  , toExpr2
  , fromExpr2
  , encode
  , encodeStr
  , encodeBase94
  , decodeBase94
  , humanToCult
  , cultToHuman
  , fvs
  , renameBoundVariables
  ) where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
-- import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.List (unfoldr)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import ICFPCString

type Expr = Expr' ByteString

data Expr' a
  = EBool !Bool
  | EInt !Integer
  | EStr a
  | EUnary !UOp (Expr' a)
  | EBinary !BinOp (Expr' a) (Expr' a)
  | EIf (Expr' a) (Expr' a) (Expr' a)
  | ELambda !Var (Expr' a)
  | ELambdaVars ![Var] (Expr' a)
  | EVar !Var
  deriving (Eq, Show, Read, Functor)

type Expr2 = Expr' ICFPCString

toExpr2 :: Expr -> Expr2
toExpr2 = fmap toICFPCString

fromExpr2 :: Expr2 -> Expr
fromExpr2 = fmap fromICFPCString

unELambdaVars :: Expr' a -> Expr' a
unELambdaVars e@(EBool {})              = e
unELambdaVars e@(EInt  {})              = e
unELambdaVars e@(EStr  {})              = e
unELambdaVars   (EUnary op e)           = EUnary op (unELambdaVars e)
unELambdaVars   (EBinary op e1 e2)      = EBinary op (unELambdaVars e1) (unELambdaVars e2)
unELambdaVars   (EIf e1 e2 e3)          = EIf (unELambdaVars e1) (unELambdaVars e2) (unELambdaVars e3)
unELambdaVars   (ELambda x e)           = ELambda x (unELambdaVars e)
unELambdaVars   (ELambdaVars [] e)      = unELambdaVars e  {- not reach -}
unELambdaVars   (ELambdaVars [x] e)     = ELambda x (unELambdaVars e)
unELambdaVars   (ELambdaVars (x:xs) e)  = ELambda x (unELambdaVars $ ELambdaVars xs e)
unELambdaVars e@(EVar {})               = e

-----

{- |
-- >>> e3 = ELambda 1 (ELambda 2 (ELambda 3 (EInt 1))) :: Expr
-- >>> toELambdaVars e3
-- ELambdaVars [1,2,3] (EInt 1)
-- >>> toELambdaVars (EBinary Apply (EBinary Apply e3 4) 5)
-- EBinary Apply (EBinary Apply ELambdaVars [1,2,3] (EInt 1))
 -}
toELambdaVars :: Expr' a -> Expr' a
toELambdaVars = f
  where
    f e@(EBool {})                           =  e
    f e@(EInt  {})                           =  e
    f e@(EStr  {})                           =  e
    f   (EUnary op e)                        =  EUnary op (toELambdaVars e)
    f   (EBinary op e1 e2)                   =  EBinary op (toELambdaVars e1) (toELambdaVars e2)
    f   (EIf e1 e2 e3)                       =  EIf (toELambdaVars e1) (toELambdaVars e2) (toELambdaVars e3)
    f   (ELambda x e@(ELambda {}))           =  lambdas (x:) e
    f   (ELambda x e@(ELambdaVars {}))       =  lambdas (x:) e
    f   (ELambda x e)                        =  (ELambdaVars [x] (toELambdaVars e))
    f   (ELambdaVars xs e@(ELambda {}))      =  lambdas (xs++) e
    f   (ELambdaVars xs e@(ELambdaVars {}))  =  lambdas (xs++) e
    f   (ELambdaVars xs e)                   =  (ELambdaVars xs (toELambdaVars e))
    f e@(EVar {})                            =  e

    lambdas xs (ELambda x e)       = lambdas (xs . (x:)) e
    lambdas xs (ELambdaVars ys e)  = lambdas (xs . (ys ++)) e
    lambdas xs  e                  = ELambdaVars (xs []) (toELambdaVars e)
type Var = Int


data UOp
  = Neg
  | Not
  | StrToInt
  | IntToStr
  deriving (Eq, Ord, Enum, Show, Read)


data BinOp
  = Add
  | Sub
  | Mult
  | Div
  | Mod
  | Lt
  | Gt
  | Eql
  | Or
  | And
  | Concat
  | Take
  | Drop
  | Apply
  | ApplyLazy
  | ApplyEager
  deriving (Eq, Ord, Enum, Show, Read)


type Token = ByteString

{- $setup
>>> :seti -XOverloadedStrings
 -}

encode :: Expr -> [Token]
encode (EBool b) = [encodeBool b]
encode (EInt n) = [encodeNat n]
encode (EStr s) = [encodeStr s]
encode (EUnary op e) = encodeUOp op : encode e
encode (EBinary op e1 e2) = encodeBinOp op : encode e1 ++ encode e2
encode (EIf e1 e2 e3) = tokenIf : encode e1 ++ encode e2 ++ encode e3
encode (ELambda x e) = encodeLambda x : encode e
encode e@(ELambdaVars {}) = encode $ unELambdaVars e
encode (EVar x) = [encodeVar x]


encodeBool :: Bool -> Token
encodeBool True = "T"
encodeBool False = "F"


{- |
>>> encodeNat 1337
"I/6"
 -}
encodeNat :: Integer -> Token
encodeNat n = "I" <> encodeBase94 n


{- |
>>> encodeStr "Hello World!"
"SB%,,/}Q/2,$_"
 -}
encodeStr :: ByteString -> Token
encodeStr s = "S" <> humanToCult s

humanToCult :: ByteString -> ByteString
humanToCult s = BS.pack [table Map.! c | c <- BS.unpack s]
  where
    table :: Map Char Char
    table = Map.fromList $
      zip "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"#$%&'()*+,-./:;<=>?@[\\]^_`|~ \n"
          "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"

cultToHuman :: ByteString -> ByteString
cultToHuman s = BS.pack [table Map.! c | c <- BS.unpack s]
  where
    table :: Map Char Char
    table = Map.fromList $
      zip "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"
          "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"#$%&'()*+,-./:;<=>?@[\\]^_`|~ \n"



encodeUOp :: UOp -> Token
encodeUOp Neg = "U-"
encodeUOp Not = "U!"
encodeUOp StrToInt = "U#"
encodeUOp IntToStr = "U$"


encodeBinOp :: BinOp -> Token
encodeBinOp Add    = "B+"
encodeBinOp Sub    = "B-"
encodeBinOp Mult   = "B*"
encodeBinOp Div    = "B/"
encodeBinOp Mod    = "B%"
encodeBinOp Lt     = "B<"
encodeBinOp Gt     = "B>"
encodeBinOp Eql    = "B="
encodeBinOp Or     = "B|"
encodeBinOp And    = "B&"
encodeBinOp Concat = "B."
encodeBinOp Take   = "BT"
encodeBinOp Drop   = "BD"
encodeBinOp Apply  = "B$"
encodeBinOp ApplyLazy   = "B~"
encodeBinOp ApplyEager  = "B!"


tokenIf :: Token
tokenIf = "?"


encodeLambda :: Int -> Token
encodeLambda n = "L" <> encodeBase94 n


encodeVar :: Int -> Token
encodeVar n = "v" <> encodeBase94 n


encodeBase94 :: Integral a => a -> ByteString
encodeBase94 n | n < 0 = undefined
encodeBase94 0 = "!"
encodeBase94 n = BS.pack (reverse (unfoldr f n))
  where
     f 0 = Nothing
     f x =
       case x `divMod` 94 of
         (q, r) -> Just (toEnum (fromIntegral (r + 33)), q)


decodeBase94 :: Integral a => ByteString -> a
decodeBase94 b =  sum $ zipWith (*) (iterate (94*) 1) $ reverse [fromIntegral (fromEnum c - fromEnum '!') | c <- BS.unpack b]


-- | Free variables
fvs :: Expr -> IntSet
fvs (EBool _) = IntSet.empty
fvs (EInt _) = IntSet.empty
fvs (EStr _) = IntSet.empty
fvs (EUnary _ e) = fvs e
fvs (EBinary _ e1 e2) = fvs e1 `IntSet.union` fvs e2
fvs (EIf e1 e2 e3) = IntSet.unions $ map fvs [e1, e2, e3]
fvs (ELambda v e) = IntSet.delete v (fvs e)
fvs e@(ELambdaVars {}) = fvs (unELambdaVars e)
fvs (EVar v) = IntSet.singleton v


-- | vs に指定した変数を束縛しないように Expr をα-変換する
renameBoundVariables :: IntSet -> Expr -> Expr
renameBoundVariables vs = f (IntMap.fromList [(v, v) | v <- IntSet.toAscList vs])
  where
    f _ e@(EBool _) = e
    f _ e@(EInt _) = e
    f _ e@(EStr _) = e
    f m (EUnary op e) = EUnary op (f m e)
    f m (EBinary op e1 e2) = EBinary op (f m e1) (f m e2)
    f m (EIf e1 e2 e3) = EIf (f m e1) (f m e2) (f m e3)
    f m (ELambda v e)
      | v `IntMap.member` m =
          let v2 = fst (IntMap.findMax m) + 1
              m2 = IntMap.insert v v2 $ IntMap.insert v2 v2 $ m
           in ELambda v2 (f m2 e)
      | otherwise =
          ELambda v (f (IntMap.insert v v m) e)
    f m e@(ELambdaVars {}) = f m (unELambdaVars e)
    f m (EVar v) =
      case IntMap.lookup v m of
        Just v2 -> EVar v2
        Nothing -> EVar v

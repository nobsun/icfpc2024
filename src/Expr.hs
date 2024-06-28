{-# LANGUAGE OverloadedStrings #-}
module Expr
  ( Expr (..)
  , Var
  , UOp (..)
  , BinOp (..)
  , Token
  , encode
  , encodeStr
  , encodeBase94
  , decodeBase94
  , humanToCult
  , cultToHuman
  ) where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.List (unfoldr)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map


data Expr
  = EBool !Bool
  | EInt !Integer
  | EStr ByteString
  | EUnary !UOp Expr
  | EBinary !BinOp Expr Expr
  | EIf Expr !Expr Expr
  | ELambda !Var Expr
  | EVar !Var
  deriving Show


type Var = Int


data UOp
  = Neg
  | Not
  | StrToInt
  | IntToStr
  deriving (Eq, Ord, Enum, Show)


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
  deriving (Eq, Ord, Enum, Show)


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
  where
    table :: Map Char Char
    table = Map.fromList $
      zip "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"#$%&'()*+,-./:;<=>?@[\\]^_`|~ \n"
          "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"

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

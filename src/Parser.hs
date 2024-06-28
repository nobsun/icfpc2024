{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Control.Applicative
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Void
import Text.Megaparsec as P

import Expr


type Parser = Parsec Void [ByteString] 


bool :: Parser Bool
bool = (single "T" *> pure True) <|> (single "F" *> pure False)


int :: Parser Integer
int = try $ do
  b <- anySingle
  case BS.stripPrefix "I" b of
    Nothing -> empty
    Just bs -> pure (decodeBase94 bs)


str :: Parser ByteString
str = try $ do
  b <- anySingle
  case BS.stripPrefix "S" b of
    Nothing -> empty
    Just bs -> pure $ BS.pack $ [table Map.! c | c <- BS.unpack bs]
  where
    table :: Map Char Char
    table = Map.fromList $
      zip "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"
          "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"#$%&'()*+,-./:;<=>?@[\\]^_`|~ \n"


uOp :: Parser UOp
uOp = asum
  [ single "U-" *> pure Neg
  , single "U!" *> pure Not
  , single "U#" *> pure StrToInt
  , single "U$" *> pure IntToStr
  ]


binOp :: Parser BinOp
binOp = asum
  [ single "B+" *> pure Add
  , single "B-" *> pure Sub
  , single "B*" *> pure Mult
  , single "B/" *> pure Div
  , single "B%" *> pure Mod
  , single "B<" *> pure Lt
  , single "B>" *> pure Gt
  , single "B=" *> pure Eql
  , single "B|" *> pure Or
  , single "B&" *> pure And
  , single "B." *> pure Concat
  , single "BT" *> pure Take
  , single "BD" *> pure Drop
  , single "B$" *> pure Apply
  ]


lam :: Parser Int
lam = do
  b <- anySingle
  case BS.stripPrefix "L" b of
    Nothing -> empty
    Just bs -> pure (decodeBase94 bs)


var :: Parser Int
var = do
  b <- anySingle
  case BS.stripPrefix "v" b of
    Nothing -> empty
    Just bs -> pure (decodeBase94 bs)


expr :: Parser Expr
expr = asum
  [ EBool <$> bool
  , EInt <$> int
  , EStr <$> str
  , do op <- uOp
       e <- expr
       pure $ EUnary op e
  , do op <- binOp
       e1 <- expr
       e2 <- expr
       pure $ EBinary op e1 e2
  , do _ <- single "?"
       c <- expr
       t <- expr
       e <- expr
       pure $ EIf c t e
  , do v <- lam
       e <- expr
       pure $ ELambda v e
  , EVar <$> var
  ]

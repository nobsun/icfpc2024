{-# LANGUAGE OverloadedStrings #-}

module CustomParser
  ( ICFPError
  , parseExpr_
  , parseExpr
  ---
  , parse, expr
  , bool, int, str
  , var, lam
  )where

import Control.Applicative
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
-- import Data.Void

import ParserLib hiding (parse)
import Expr

{- $setup
>>> :seti -XOverloadedStrings
>>> import qualified Data.ByteString.Char8 as B8
>>> let parse' p = parse p . B8.words
 -}

bool :: Parser Bool
bool = (single "T" *> pure True) <|> (single "F" *> pure False)

{- |
>>> parse' int "I/6"
Right 1337
 -}
int :: Parser Integer
int = try $ do
  b <- anySingle
  case BS.stripPrefix "I" b of
    Nothing -> empty
    Just bs -> pure (decodeBase94 bs)


{- |
>>> parse' str "SB%,,/}Q/2,$_"
Right "Hello World!"
 -}
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
lam = try $ do
  b <- anySingle
  case BS.stripPrefix "L" b of
    Nothing -> empty
    Just bs -> pure (decodeBase94 bs)


var :: Parser Int
var = try $ do
  b <- anySingle
  case BS.stripPrefix "v" b of
    Nothing -> empty
    Just bs -> pure (decodeBase94 bs)


{- |
>>> parse' expr "U- I$"
Right (EUnary Neg (EInt 3))
>>> parse' expr "U! T"
Right (EUnary Not (EBool True))
>>> parse' expr "U# S4%34"
Right (EUnary StrToInt (EStr "test"))
>>> parse' expr "U$ I4%34"
Right (EUnary IntToStr (EInt 15818151))
>>> parse' expr "B+ I# I$"
Right (EBinary Add (EInt 2) (EInt 3))
>>> parse' expr "B- I$ I#"
Right (EBinary Sub (EInt 3) (EInt 2))
>>> parse' expr "B* I$ I#"
Right (EBinary Mult (EInt 3) (EInt 2))
 -}
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

---

{- parse-error type for ICFP language  -}
type ICFPError = String

parse :: WParser ByteString a -> ByteString -> Either ICFPError (a, [ByteString])
parse p = runParser p . BS.words

parseExpr_ :: ByteString -> Either ICFPError Expr
parseExpr_ = evalParser (expr <* eof) . BS.words

parseExpr :: String -> BS.ByteString -> Either ICFPError Expr
parseExpr _dummy = parseExpr_

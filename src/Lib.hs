-- # Lib 雛形モジュール
-- ## 言語拡張と`module`宣言の雛形
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE LambdaCase, MultiWayIf #-}
{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE DataKinds, PolyKinds, NoStarIsType, TypeFamilyDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot, NoFieldSelectors, DuplicateRecordFields #-}
module Lib
    ( someFunc
    ) where

import Data.Char (chr, ord)
import Data.Maybe (mapMaybe)

{- |
「なんか関数」を標準出力に印字する
>>> someFunc
なんか関数
-}
someFunc :: IO ()
someFunc = putStrLn "なんか関数"

{- |
>>> int "/6"
1337
-}
int :: String -> Int
int = foldl (\acc c -> acc * 94 + ord c - ord '!') 0

-- str :: String -> String
str = mapMaybe $ flip lookup dict

dict = zip cultCode humanReadable
  where
    cultCode = map chr [33..126]
    humanReadable = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"#$%&'()*+,-./:;<=>?@[\\]^_`|~ \n"

-- hello world
test = "B%,,/}Q/2,$_"

-- curl -X POST -H 'Authorization: Bearer 39572a1c-b861-4e57-8405-b9fda4f8cec3' https://boundvariable.space/communicate
communicate = "J!23%}%22/2n}O.%80%#4%$}?I@"

module LangICFP where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Array

import Imports

decodeFile :: FilePath -> IO String
decodeFile = (either fail pure . decodeString =<<) . readFile

encodeFile :: FilePath -> IO String
encodeFile = (either fail pure . encodeString =<<) . readFile

-- toReadable :: String -> Either String String
-- toReadable = decodeString

-- fromReadable :: String -> Either String String
-- fromReadable = encodeString

---

data Exp
  = EBool Bool
  | EInt Integer
  | EString String
  deriving Show

---

decodeBool :: Char -> String -> Maybe Exp
decodeBool s _ = case s of
  'T' -> Just $ EBool True
  'F' -> Just $ EBool False
  _   -> Nothing

---

{- |
>>> decodeInt "/6"
Right 1337
 -}
decodeInt :: String -> Either String Integer
decodeInt s = foldl' base 0 . map fromIntegral <$> zipWithM decodeDigit [1 :: Int ..] s
  where base a x = a * 94 + x

{- |
>>> encodeInt 1337
"/6"
 -}
encodeInt :: Integer -> String
encodeInt i0 = map (arrayFromDigit !) $ go [] i0
  where go a i  | q > 0 || q == 0 && r > 0  = go (fromIntegral r:a) q
                | otherwise                 = a
          where (q, r) = i `quotRem` 94

decodeDigit :: Show a => a -> Char -> Either String Word8
decodeDigit ix c = maybe (Left $ "int decode: unknown char " ++ show c ++ " at " ++ show ix) Right $ decodeDigit' c

decodeDigit' :: Char -> Maybe Word8
decodeDigit' c = Map.lookup c mapToDigit

encodeDigit :: (Show ix, Ord a, Integral a, Show a) => ix -> a -> Either String Char
encodeDigit ix d = maybe (Left $ "string encode: out-of-range digit " ++ show d ++ " at " ++ show ix) Right $ encodeDigit' d

encodeDigit' :: (Ord a, Integral a) => a -> Maybe Char
encodeDigit' ix
  | ix < 0     = Nothing
  | 93 < ix    = Nothing
  | otherwise  = Just $ arrayFromDigit ! fromIntegral ix

mapToDigit :: Map Char Word8
mapToDigit = Map.fromList $ zip codeChars [0..93]

arrayFromDigit :: Array Int Char
arrayFromDigit = listArray (0, 93) ['!' .. '~']

-----

decodeString :: String -> Either String String
decodeString s = zipWithM decodeChar [1 :: Int ..] s

encodeString :: String -> Either String String
encodeString s = zipWithM encodeChar [1 :: Int ..] s

---

decodeChar :: Show a => a -> Char -> Either String Char
decodeChar ix c = maybe (Left $ "string decode: unknown char " ++ show c ++ " at " ++ show ix) Right $ decodeChar' c

decodeChar' :: Char -> Maybe Char
decodeChar' c = Map.lookup c mapToString

encodeChar :: Show a => a -> Char -> Either String Char
encodeChar ix c = maybe (Left $ "string encode: unknown char " ++ show c ++ " at " ++ show ix) Right $ encodeChar' c

encodeChar' :: Char -> Maybe Char
encodeChar' c = Map.lookup c mapFromString

---

mapToString :: Map Char Char
mapToString = Map.fromList $ zip codeChars stringChars

mapFromString :: Map Char Char
mapFromString = Map.fromList $ zip stringChars codeChars

stringChars :: String
stringChars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"#$%&'()*+,-./:;<=>?@[\\]^_`|~ \n"

---

codeChars :: String
codeChars = ['!' .. '~'] {- 33 - 126 -}

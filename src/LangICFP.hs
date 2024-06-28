module LangICFP where

import Data.Map (Map)
import qualified Data.Map as Map

import Imports

decodeFile :: FilePath -> IO String
decodeFile = (either fail pure . decode =<<) . readFile

encodeFile :: FilePath -> IO String
encodeFile = (either fail pure . encode =<<) . readFile

---

toReadable :: String -> Either String String
toReadable = decode

decode :: String -> Either String String
decode s = zipWithM decodeChar [1 :: Int ..] s

fromReadable :: String -> Either String String
fromReadable = encode

encode :: String -> Either String String
encode s = zipWithM encodeChar [1 :: Int ..] s

---

decodeChar' :: Char -> Maybe Char
decodeChar' c = Map.lookup c mapToReadable

decodeChar :: Show a => a -> Char -> Either String Char
decodeChar ix c = maybe (Left $ "icfp decode: unknown char " ++ show c ++ " at " ++ show ix) Right $ decodeChar' c

encodeChar' :: Char -> Maybe Char
encodeChar' c = Map.lookup c mapFromReadable

encodeChar :: Show a => a -> Char -> Either String Char
encodeChar ix c = maybe (Left $ "icfp encode: unknown char " ++ show c ++ " at " ++ show ix) Right $ encodeChar' c

---

mapToReadable :: Map Char Char
mapToReadable = Map.fromList $ zip boundVariableCult humanReadable

mapFromReadable :: Map Char Char
mapFromReadable = Map.fromList $ zip humanReadable boundVariableCult

boundVariableCult :: String
boundVariableCult = map chr [33..126]

humanReadable :: String
humanReadable = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"#$%&'()*+,-./:;<=>?@[\\]^_`|~ \n"

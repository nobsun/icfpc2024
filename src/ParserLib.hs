{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ParserLib where

import Control.Monad.Trans.Except (throwE)
-- import Data.ByteString (ByteString)

import Imports

type Parser = WParser ByteString

parse :: Parser a -> [ByteString] -> Either String a
parse = evalParser

anySingle :: Parser ByteString
anySingle = token

single :: ByteString -> Parser ByteString
single x = satisfy (== x)

try :: Parser a -> Parser a
try = id

-----

type Error = Last String

runError :: Error -> String
runError = fromMaybe "<empty error>" . getLast

newtype WParser w a = Parser (StateT [w] (Except Error) a)
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus)

runParser :: WParser w a -> [w] -> Either String (a, [w])
runParser (Parser p) ws = either (Left . runError) Right $ runExcept (runStateT p ws)

evalParser :: WParser w a -> [w] -> Either String a
evalParser p ws = fst <$> runParser p ws

raise_ :: String -> StateT [w] (Except Error) a
raise_ = lift . throwE . Last . Just

raise :: String -> WParser w a
raise = Parser . raise_

token :: WParser w w
token = Parser $ do
  input <- get
  case input of
    []    -> raise_ "ParserLib.token: no more inputs"
    w:ws  -> put ws $> w

eof :: Show w => WParser w ()
eof = Parser $ do
  input <- get
  case input of
    []    -> pure ()
    _:_   -> raise_ $ "ParserLib.eof: not empty, rest of inputs: " ++ show (take 10 input) ++ " ..."

satisfy :: (w -> Bool) -> WParser w w
satisfy p = do
  w <- token
  guard $ p w
  pure w

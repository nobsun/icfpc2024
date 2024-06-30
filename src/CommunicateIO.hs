{-# LANGUAGE OverloadedStrings #-}

module CommunicateIO where

import qualified Data.ByteString.Char8 as B8
-- import qualified Data.ByteString.Lazy.Char8 as L8
import System.Process (readProcess)

import Imports hiding (get)
import CustomParser (parseExpr_)
import Expr
import Pretty (pprInfix)

communicateFile :: FilePath -> IO String
communicateFile = (communicateRaw =<<) . readFile

communicateRaw :: String -> IO String
communicateRaw s = readProcess "./api/comm.sh" [] s

communicate :: String -> IO String
communicate s = communicateRaw . unwords . map B8.unpack . encode . EStr $ fromString s

communicateExpr :: String -> IO Expr
communicateExpr s = either fail pure . parseExpr_ . B8.pack =<< communicate s

command :: String -> [String] -> String
command cmd args = unwords $ cmd : args

getExpr :: String -> IO Expr
getExpr arg = communicateExpr $ command "get" [arg]

get :: String -> IO Expr
get = getExpr

getLang :: String -> IO String
getLang arg = communicate $ command "get" [arg]

getPrint :: String -> IO ()
getPrint arg = do
  expr <- get arg
  case expr of
    EStr s  -> B8.putStr s
    _       -> print expr

getPpr :: String -> IO ()
getPpr arg = putStrLn . pprInfix =<< get arg

echo :: String -> IO Expr
echo s = communicateExpr (command "echo" [s])

solve :: String -> String -> IO Expr
solve prob solution = communicateExpr (command "solve" [prob, solution])

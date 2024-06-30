{-# LANGUAGE OverloadedStrings #-}

module CommunicateIO where

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as L8
import System.Process (readProcess)

import Imports hiding (get)
import CustomParser (parseExpr_)
import Expr
import Pretty (pprInfix)

communicateFile :: FilePath -> IO String
communicateFile = (communicate_ =<<) . L8.readFile

communicate :: L8.ByteString -> IO Expr
communicate req = either fail pure . parseExpr_ . B8.pack =<< communicate_ req

communicate_ :: L8.ByteString -> IO String
communicate_ bs = readProcess "./api/comm.sh" [] (L8.unpack bs)

command :: String -> String -> IO Expr
command cmd x = communicate $ L8.fromChunks $ encode $ EStr $ fromString $ cmd <> " " <> x

get :: String -> IO Expr
get = command "get"

get' :: String -> IO ()
get' arg = do
  expr <- get arg
  case expr of
    EStr s  -> B8.putStr s
    _       -> print expr

getPpr :: String -> IO ()
getPpr arg = putStrLn . pprInfix =<< get arg

echo :: String -> IO Expr
echo = command "echo"

solve :: String -> String -> IO Expr
solve prob solution = command "solve" $ unwords [prob, solution]

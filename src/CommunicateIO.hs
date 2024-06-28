{-# LANGUAGE OverloadedStrings #-}

module CommunicateIO where

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as L8
import System.Process (readProcess)

import Imports hiding (get)
import CustomParser (parseExpr)
import Expr

communicateFile :: FilePath -> IO String
communicateFile = (communicate_ =<<) . L8.readFile

communicate :: L8.ByteString -> IO Expr
communicate req = either fail pure . parseExpr . B8.pack  =<< communicate_ req

communicate_ :: L8.ByteString -> IO String
communicate_ = readProcess "./api/comm.sh" [] . L8.unpack

get :: String -> IO Expr
get x = communicate $ L8.fromChunks $ encode $ EStr $ "get " <> fromString x

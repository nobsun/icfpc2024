{-# LANGUAGE OverloadedStrings #-}
module Client where

import Data.Char (chr, ord)
import Data.Maybe (mapMaybe)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Client (defaultManagerSettings, RequestBody (RequestBodyLBS))

import Expr
import Parser (parseExpr)

token = "Bearer 39572a1c-b861-4e57-8405-b9fda4f8cec3"


simplePost raw = do
  let msg = encodeStr $ BS.pack raw
  print "===================="
  print $ BS.pack "SEND: " <> msg
  print "===================="
  manager <- newManager tlsManagerSettings
  initReq <- parseRequest "https://boundvariable.space/communicate"
  let req = initReq { method = "POST"
                    , requestHeaders = [( "Authorization", token)]
                    , requestBody = RequestBodyBS msg
                    }
  resp <- httpLbs req manager
  let body = LBS.toStrict $ responseBody resp
  writeFile "response.txt" $ BS.unpack body
  print "===================="
  print $ BS.pack "RECV: " <> body
  print "===================="
  return $ parseExpr "simple" $ BS.words body

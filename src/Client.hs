{-# LANGUAGE OverloadedStrings #-}
module Client where

import Data.Char (chr, ord, isSpace)
import Data.Maybe (mapMaybe)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Client (defaultManagerSettings, RequestBody (RequestBodyLBS))

import Expr
import Parser (parseExpr)

simplePost raw = do
  token <- BS.dropWhileEnd isSpace <$> BS.dropWhile isSpace <$> BS.readFile "token.txt"

  let msg = encodeStr $ BS.pack raw
  putStrLn "===================="
  BS.putStrLn $ "SEND: " <> msg
  putStrLn "===================="
  manager <- newManager tlsManagerSettings
  initReq <- parseRequest "https://boundvariable.space/communicate"
  let req = initReq { method = "POST"
                    , requestHeaders = [( "Authorization", "Bearer " <> token)]
                    , requestBody = RequestBodyBS msg
                    }
  resp <- httpLbs req manager
  let body = LBS.toStrict $ responseBody resp
  BS.writeFile "response.txt" body
  putStrLn "===================="
  BS.putStrLn $ "RECV: " <> body
  putStrLn "===================="

  let ret = parseExpr "simple" body
  case ret of
    Left err -> do
      print err
      putStrLn "===================="
    Right e -> do
      let expr_repr = show e
      writeFile "response_expr.txt" expr_repr
      case e of
        EStr s -> do
          BS.putStrLn s
          putStrLn "===================="
          BS.writeFile "response_decoded_str.txt" s
        _ -> return ()

  return ret

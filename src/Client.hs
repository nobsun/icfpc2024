{-# LANGUAGE OverloadedStrings #-}
module Client where

import Control.Exception
import Data.Char (chr, ord, isSpace)
import Data.Maybe (mapMaybe)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Void
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Client (defaultManagerSettings, RequestBody (RequestBodyLBS))
import Text.Megaparsec (ParseErrorBundle)

import Expr
import Eval (evalExpr)
import Parser (parseExpr)
import qualified Value

simplePost raw = do
  token <- readToken

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

cat :: String -> IO ()
cat name = do
  s <- getString name
  BS.putStr s

getString :: String -> IO BS.ByteString
getString name = do
  expr <- getExpr name
  case Value.eval expr of
    Value.VStr s -> return s
    Value.VBool _ -> throwIO (userError "evaluated to boolean")
    Value.VInt _ -> throwIO (userError "evaluated to int")
    Value.VFun _ -> throwIO (userError "evaluated to function")

getExpr :: String -> IO Expr
getExpr name = do
  ret <- postRaw ("get " ++ name)
  case ret of
    Left err -> throwIO $ userError $ show err
    Right expr -> return expr

download :: String -> IO ()
download name = do
  s <- getString name
  BS.writeFile ("answers/" ++ name) s

-- Usage:
--
-- > submitSolution "lambdaman1" "LLLDURRRUDRRURR"
-- EStr "Correct, you solved lambdaman1 with a score of 33!\n"
submitSolution :: String -> String -> IO Expr
submitSolution name solution = do
  ret <- postRaw ("solve " ++ name ++ " " ++ solution)
  case ret of
    Left err -> throwIO $ userError $ show err
    Right expr -> return expr
 
readToken :: IO BS.ByteString
readToken = BS.dropWhileEnd isSpace <$> BS.dropWhile isSpace <$> BS.readFile "token.txt"

postRaw :: String -> IO (Either (ParseErrorBundle [BS.ByteString] Void) Expr)
postRaw raw = do
  token <- readToken

  let msg = encodeStr $ BS.pack raw
  manager <- newManager tlsManagerSettings
  initReq <- parseRequest "https://boundvariable.space/communicate"
  let req = initReq { method = "POST"
                    , requestHeaders = [( "Authorization", "Bearer " <> token)]
                    , requestBody = RequestBodyBS msg
                    }
  resp <- httpLbs req manager
  let body = LBS.toStrict $ responseBody resp
  return $ parseExpr "simple" body

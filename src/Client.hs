{-# LANGUAGE OverloadedStrings #-}
module Client where

import Control.Exception
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
-- import Data.Void
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
-- import Text.Megaparsec (ParseErrorBundle)

import Imports
import Expr
-- import Eval (evalExpr)
import Parser (ICFPError, parseExpr)
import qualified Value
import SpaceShip

simplePost :: String -> IO (Either ICFPError Expr)
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
  ret <- postString ("get " ++ name)
  case ret of
    Left err -> throwIO $ userError $ show err
    Right expr -> return expr

download :: String -> IO ()
download name = do
  s <- getString name
  BS.writeFile ("answers/" ++ name) s

downloadExpr :: String -> IO ()
downloadExpr name = do
  e <- getExpr name
  writeFile ("answers/" ++ name) (show e)

-- Usage:
--
-- > submitSolution "lambdaman1" "LLLDURRRUDRRURR"
-- Correct, you solved lambdaman1 with a score of 33!
submitSolution :: String -> String -> IO ()
submitSolution name solution = do
  ret <- postString ("solve " ++ name ++ " " ++ solution)
  case ret of
    Left err -> throwIO $ userError $ show err
    Right expr -> do
      case Value.eval expr of
        Value.VStr s  -> BS.putStr s
        Value.VBool b -> print b
        Value.VInt n  -> print n
        Value.VFun _  -> BS.putStrLn "<function>"

submitSolutionBis :: String -> String -> IO ()
submitSolutionBis name solution = do
  ret <- postString ("solve " ++ name ++ " " ++ solution)
  case ret of
    Left err -> throwIO $ userError $ show err
    Right expr -> do
      case Value.eval expr of
        Value.VStr s  -> output name $ BS.unpack s
        Value.VBool b -> output name $ show b
        Value.VInt n  -> output name $ show n
        Value.VFun _  -> output name "<function>"
  where
    output n = writeFile ("solutions/" ++ n ++ "/solv_resolv.txt")

-- Usage
--
-- > submitExpr "lambdaman1" (EStr "LLLDURRRUDRRURR")
-- Correct, you solved lambdaman1 with a score of 38!
submitExpr :: String -> Expr -> IO ()
submitExpr name solution = do
  ret <- postExpr $ EBinary Concat (EStr ("solve " <> BS.pack name <> " ")) solution
  case ret of
    Left err -> throwIO $ userError $ show err
    Right expr -> do
      case Value.eval expr of
        Value.VStr s  -> BS.putStr s
        Value.VBool b -> print b
        Value.VInt n  -> print n
        Value.VFun _  -> BS.putStrLn "<function>"

readToken :: IO BS.ByteString
readToken = BS.dropWhileEnd isSpace <$> BS.dropWhile isSpace <$> BS.readFile "token.txt"

postString :: String -> IO (Either ICFPError Expr)
postString s = postExpr (EStr (BS.pack s))

postExpr :: Expr -> IO (Either ICFPError Expr)
postExpr expr = do
  token <- readToken

  let msg = BS.unwords (encode expr)
  manager <- newManager tlsManagerSettings
  initReq <- parseRequest "https://boundvariable.space/communicate"
  let req = initReq { method = "POST"
                    , requestHeaders = [( "Authorization", "Bearer " <> token)]
                    , requestBody = RequestBodyBS msg
                    }
  resp <- httpLbs req manager
  let body = LBS.toStrict $ responseBody resp
  return $ parseExpr "simple" body

----
-- | Naive と NearbySorted の結果を比較して、改善があれば提出する
check :: FilePath -> IO ()
check prob = do
  old <- solveFileBy (solveBy solveNaive) ("answers/" ++ prob)
  new <- solveFileBy (solveBy solveNearbySorted) ("answers/" ++ prob)
  print $ "Old: " ++ show (length old)
  print $ "New: " ++ show (length new)
  if length old > length new
    then do
      putStrLn "\"Improved\""
      submitSolution prob new
    else
      putStrLn "\"No improvement\""

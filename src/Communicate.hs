module Communicate where

import qualified Data.ByteString.Lazy.Char8 as L8
import System.Process (readProcess)

communicateFile :: FilePath -> IO String
communicateFile = (communicate =<<) . L8.readFile

communicate :: L8.ByteString -> IO String
communicate = readProcess "./api/comm.sh" [] . L8.unpack

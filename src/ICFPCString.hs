{-# LANGUAGE GHC2021 #-}
module ICFPCString
    ( ICFPCString
    , toICFPCString
    , fromICFPCString
    ) where

import qualified Data.ByteString.Char8 as BS
import Data.Char
import Data.Foldable
import Data.Sequence
import Data.Word

type ICFPCString = Seq Word8

toICFPCString :: BS.ByteString -> ICFPCString
toICFPCString = fromList . map (fromIntegral . ord) . BS.unpack

fromICFPCString :: ICFPCString -> BS.ByteString
fromICFPCString = BS.pack . map (chr . fromIntegral) . toList


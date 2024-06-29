{-# LANGUAGE GHC2021 #-}
module ICFPCString
    (
    ) where

import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char
import Data.Foldable
import Data.Sequence
import Data.Word

type ICFPCString = Seq Word8

toICFPCString :: B.ByteString -> ICFPCString
toICFPCString = fromList . map (fromIntegral . ord) . B.unpack

fromICFPCString :: ICFPCString -> B.ByteString
fromICFPCString = B.pack . map (chr . fromIntegral) . toList


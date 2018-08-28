{-# OPTIONS_GHC -Wall #-}

module Util.ByteString
( bsChunksOf
) where

import Data.List (unfoldr)
import qualified Data.ByteString as BS

bsChunksOf :: Int -> BS.ByteString -> [BS.ByteString]
bsChunksOf x = unfoldr gen
  where
    gen a | BS.null a = Nothing
          | otherwise = Just (BS.splitAt x a)


{-# OPTIONS_GHC -Wall #-}

module Utils (chunksOf) where

import Data.List (unfoldr)
import qualified Data.ByteString as BS


chunksOf :: Int -> BS.ByteString -> [BS.ByteString]
chunksOf x = unfoldr gen
  where
    gen a | BS.null a = Nothing
          | otherwise = Just (BS.splitAt x a)


{-# OPTIONS_GHC -Wall #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Util.ByteString
-- Copyright   :  (c) Georgios Karachalias, 2018
-- License     :  BSD3
--
-- Maintainer  :  gdkaracha@gmail.com
-- Stability   :  experimental
-- Portability :  GHC
--
-- Utility functions operating on ByteStrings.
--
-----------------------------------------------------------------------------

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


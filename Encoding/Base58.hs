{-# OPTIONS_GHC -Wall #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Encoding.Base58
-- Copyright   :  (c) Georgios Karachalias, 2018
-- License     :  BSD3
--
-- Maintainer  :  gdkaracha@gmail.com
-- Stability   :  experimental
-- Portability :  GHC
--
-- Base58 encoding.
--
-----------------------------------------------------------------------------

module Encoding.Base58
( -- * Base58 encoding
  encodeBase58

  -- * Base58 decoding
, decodeBase58
) where

import Data.Array (Ix, Array, (!), listArray)
import qualified Data.Map as M

-- * Encoding into Base58
-- ----------------------------------------------------------------------------

-- | Base58 encoding
encodeBase58 :: (Ix i, Integral i) => i -> String
encodeBase58 m | m == 0    = "1"      -- special case
               | otherwise = aux m ""
  where
    aux :: (Ix a, Integral a) => a -> String -> String
    aux n acc | n > 0, (d,r) <- quotRem n 58 = aux d ((encodeTable ! r) : acc)
              | otherwise = acc

    encodeTable :: (Ix i, Integral i) => Array i Char
    encodeTable = listArray (0,57)
                    "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

-- * Decoding from Base58
-- ----------------------------------------------------------------------------

-- | Base58 decoding
decodeBase58 :: Integral a => String -> Maybe a
decodeBase58 = aux 0
  where
    aux :: Integral b => b -> String -> Maybe b
    aux acc []     = Just acc
    aux acc (c:cs) | Just i <- M.lookup c decodeMap = aux (acc*58 + i) cs
                   | otherwise                      = Nothing

    decodeMap :: Integral a => M.Map Char a
    decodeMap = M.fromList
              $ zip "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
                    [0..57]


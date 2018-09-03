{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Words
-- Copyright   :  (c) Georgios Karachalias, 2018
-- License     :  ???
--
-- Maintainer  :  gdkaracha@gmail.com
-- Stability   :  experimental
-- Portability :  GHC
--
-- Unsigned integer types and standard operations on them.
--
-----------------------------------------------------------------------------

module Data.Words
( -- * Bitwise operations for integers
  Bits(..)

  -- * Word partitioning
, ByteLength(..), Bytes(..), toBytesGen, fromBytesGen
, WordLength(..), Words(..), toWordsGen, fromWordsGen
, Split(..)

  -- * Unsigned integral types
, Word8, Word16, Word32, Word64, Word128, Word160, Word256, Word512, BS.ByteString

  -- * ByteString-Integer conversions
, bsToInteger, integerToBytes, integerToBS, fromByteString, fromByteStringUnsafe

  -- * Misc
, revWord32
) where

import Data.Word
import Data.Word.Word128 (Word128)
import Data.Word.Word160 (Word160)
import Data.Word.Word256 (Word256)
import Data.Word.Word512 (Word512)
import qualified Data.ByteString as BS

import Data.Word.Partition ( ByteLength(..), Bytes(..)
                           , toBytesGen, fromBytesGen
                           , WordLength(..), Words(..)
                           , toWordsGen, fromWordsGen
                           , Split(..) )

import Data.Bits (Bits(..))
import Data.List (foldl')

-- * Parse ByteStrings as words
-- ----------------------------------------------------------------------------

-- | Convert a 'ByteString' into an 'Integer'.
bsToInteger :: BS.ByteString -> Integer
bsToInteger = foldl' (\a w -> (a `shiftL` 8) .|. fromIntegral w) 0 . BS.unpack

-- | Convert an 'Integer' into a list of bytes.
integerToBytes :: Integer -> [Word8]
integerToBytes 0 = [0x00]
integerToBytes i = aux [] i
  where
    aux acc 0 = acc
    aux acc n | (q,r) <- quotRem n 256
              = aux (fromIntegral r : acc) q

-- | Convert an 'Integer' into a 'ByteString'.
integerToBS :: Integer -> BS.ByteString
integerToBS = fromBytes . integerToBytes

fromByteString :: (Bytes a, Num a) => BS.ByteString -> Maybe a
fromByteString bs
  | BS.length bs == noBytes result = Just result
  | otherwise                      = Nothing
  where
    result = fromInteger (bsToInteger bs)

fromByteStringUnsafe :: (Bytes a, Num a) => BS.ByteString -> a
fromByteStringUnsafe bs = case fromByteString bs of
  Just word -> word
  Nothing   -> error "fromByteStringUnsafe: Nothing"

-- * Misc
-- ----------------------------------------------------------------------------

revWord32 :: Word32 -> Word32
revWord32 = fromBytes . reverse . toBytes


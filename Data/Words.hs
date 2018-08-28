{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeFamilies #-}

-- Re-export all word types
module Data.Words
( module Data.Bits
, module Utils.Bytes
, module Utils.Words
, module Utils.Split
, Word8, Word16, Word32, Word64, Word128, Word160, Word256, Word512, BS.ByteString
, bsToInteger, integerToBytes, integerToBS, fromByteString, fromByteStringUnsafe
, revWord32 -- TODO: ADD MORE OPERATIONS AND REMOVE THIS ONE
) where

import Data.Word
import Data.Words.Word128 (Word128)
import Data.Words.Word160 (Word160)
import Data.Words.Word256 (Word256)
import Data.Words.Word512 (Word512)
import qualified Data.ByteString as BS

import Utils.Bytes
import Utils.Words
import Utils.Split

import Data.Bits
import Data.List (foldl')

-- * Parse ByteStrings as words
-- ----------------------------------------------------------------------------

bsToInteger :: BS.ByteString -> Integer
bsToInteger = foldl' (\a w -> (a `shiftL` 8) .|. fromIntegral w) 0 . BS.unpack

integerToBytes :: Integer -> [Word8]
integerToBytes 0 = [0x00]
integerToBytes i = aux [] i
  where
    aux acc 0 = acc
    aux acc n | (q,r) <- quotRem n 256
              = aux (fromIntegral r : acc) q

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

-- * Encoding Changes
-- ----------------------------------------------------------------------------

revWord32 :: Word32 -> Word32
revWord32 = fromBytes . reverse . toBytes

-- TODO: Big endian for bits
-- TODO: Little endian for bytes


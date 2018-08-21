{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeFamilies, FlexibleContexts #-}

-- Re-export all word types
module Words
( module Data.Bits
, Word8, Word16, Word32, Word64, Word128, Word160, Word256, Word512
, Split(..), Bytes(..), fromByteString, fromByteStringUnsafe, Words(..)
) where

import Data.Word
import Data.Bits
import qualified Data.ByteString as BS
import Data.List (foldl')
import Word128
import Word160
import Word256
import Word512

-- * Splitting words in half
-- ----------------------------------------------------------------------------

-- | Split a 2n-long word into two n-long ones
class Split a where
  type Half a
  toHalves   :: a -> (Half a, Half a)
  fromHalves :: (Half a, Half a) -> a

instance Split Word16 where
  type Half Word16 = Word8
  toHalves w = (pt1,pt2)
    where
      pt1 = fromIntegral $ (w .&. 0xFF00) `rotateR` 8
      pt2 = fromIntegral $ (w .&. 0x00FF)
  fromHalves (w1,w2) = pt1 .|. pt2
    where
      pt1 = fromIntegral w1 `rotateL` 8
      pt2 = fromIntegral w2

instance Split Word32 where
  type Half Word32 = Word16
  toHalves w = (pt1,pt2)
    where
      pt1 = fromIntegral $ (w .&. 0xFFFF0000) `rotateR` 16
      pt2 = fromIntegral $ (w .&. 0x0000FFFF)
  fromHalves (w1,w2) = pt1 .|. pt2
    where
      pt1 = fromIntegral w1 `rotateL` 16
      pt2 = fromIntegral w2

instance Split Word64 where
  type Half Word64 = Word32
  toHalves w = (pt1,pt2)
    where
      pt1 = fromIntegral $ (w .&. 0xFFFFFFFF00000000) `rotateR` 32
      pt2 = fromIntegral $ (w .&. 0x00000000FFFFFFFF)
  fromHalves (w1,w2) = pt1 .|. pt2
    where
      pt1 = fromIntegral w1 `rotateL` 32
      pt2 = fromIntegral w2

instance Split Word128 where
  type Half Word128 = Word64
  toHalves   = w128ToW64s
  fromHalves = w64sToW128

instance Split Word256 where
  type Half Word256 = Word128
  toHalves   = w256ToW128s
  fromHalves = w128sToW256

instance Split Word512 where
  type Half Word512 = Word256
  toHalves   = w512ToW256s
  fromHalves = w256sToW512

-- * Split a word into bytes
-- ----------------------------------------------------------------------------

class Bytes a where
  toBytes   :: a -> [Word8]
  fromBytes :: [Word8] -> a
  noBytes   :: a -> Int
  toByteString :: a -> BS.ByteString
  toByteString = BS.pack . toBytes -- default

instance Bytes Word8 where
  toBytes w = [w]
  fromBytes = fromBytesGen
  noBytes _ = 1

instance Bytes Word16 where
  toBytes w = [w1,w2]
    where (w1,w2) = toHalves w
  fromBytes = fromBytesGen
  noBytes _ = 2

instance Bytes Word32 where
  toBytes w = toBytes w1 ++ toBytes w2
    where (w1,w2) = toHalves w
  fromBytes = fromBytesGen
  noBytes _ = 4

instance Bytes Word64 where
  toBytes w = toBytes w1 ++ toBytes w2
    where (w1,w2) = toHalves w
  fromBytes = fromBytesGen
  noBytes _ = 8

instance Bytes Word128 where
  toBytes   = w128ToBytes
  fromBytes = fromBytesGen
  noBytes _ = 16

instance Bytes Word160 where
  toBytes   = w160ToBytes
  fromBytes = fromBytesGen
  noBytes _ = 20

instance Bytes Word256 where
  toBytes   = w256ToBytes
  fromBytes = fromBytesGen
  noBytes _ = 32

instance Bytes Word512 where
  toBytes   = w512ToBytes
  fromBytes = fromBytesGen
  noBytes _ = 64

instance Bytes BS.ByteString where
  toBytes   = BS.unpack
  fromBytes = BS.pack
  noBytes   = BS.length

-- TODO: The toBytes implementations are inefficient. CPS them

-- Generic implementation of 'fromBytes' for types whose representation is finite
fromBytesGen :: (Num b, FiniteBits b) => [Word8] -> b
fromBytesGen ws
  = ws `ensureLength` (finiteBitSize result `div` 8) `seq` result
  where result = fromInteger
               $ foldl' (\a w -> (a `rotateL` 8) .|. fromIntegral w) 0 ws

ensureLength :: [a] -> Int -> ()
ensureLength xs n
  | length xs == n = ()
  | otherwise      = error "ensureLength"

-- * Split a word into word32s
-- ----------------------------------------------------------------------------

class Words a where
  toWords   :: a -> [Word32]
  fromWords :: [Word32] -> a

instance Words Word32 where
  toWords w = [w]
  fromWords = fromWordsGen

instance Words Word64 where
  toWords w = [w1,w2]
    where (w1,w2) = toHalves w
  fromWords = fromWordsGen

instance Words Word128 where
  toWords   = toWordsGen
  fromWords = fromWordsGen

instance Words Word160 where
  toWords   = w160ToWords
  fromWords = fromWordsGen

instance Words Word256 where
  toWords   = toWordsGen
  fromWords = fromWordsGen

instance Words Word512 where
  toWords   = toWordsGen
  fromWords = fromWordsGen

-- TODO: ByteString cannot be an instance of this class cause it's not always
--       of the proper length.

-- Generic implementation of 'fromWords' for types whose representation is finite
fromWordsGen :: (Num b, FiniteBits b) => [Word32] -> b
fromWordsGen ws
  = ws `ensureLength` (finiteBitSize result `div` 32) `seq` result
  where result = fromInteger
               $ foldl' (\a w -> (a `rotateL` 32) .|. fromIntegral w) 0 ws

-- Generic implementation of 'toWords' for types that are an instance of Split
toWordsGen :: (Words (Half a), Split a) => a -> [Word32]
toWordsGen w = toWords w1 ++ toWords w2
  where
    (w1,w2) = toHalves w

-- * Parse ByteStrings as words
-- ----------------------------------------------------------------------------

fromByteString :: (Bytes a, Num a) => BS.ByteString -> Maybe a
fromByteString bs
  | BS.length bs == noBytes result = Just result
  | otherwise                      = Nothing
  where
    result = fromInteger
           $ foldl' (\a w -> (a `shiftL` 8) .|. fromIntegral w) 0 (BS.unpack bs)

fromByteStringUnsafe :: (Bytes a, Num a) => BS.ByteString -> a
fromByteStringUnsafe bs = case fromByteString bs of
  Just word -> word
  Nothing   -> error "fromByteStringUnsafe: Nothing"

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Word.Partition
( ByteLength(..)
, Bytes(..)
, toBytesGen
, fromBytesGen
--
, WordLength(..)
, Words(..)
, toWordsGen
, fromWordsGen
--
, Split(..)
) where

import Data.Word (Word8, Word16, Word32, Word64)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import qualified Data.ByteString as BS
import Data.List (unfoldr, foldl')

-- * A class for all types whose representation can be mesaured in bytes
-- ----------------------------------------------------------------------------

class ByteLength a where
  noBytes :: a -> Int

instance ByteLength Word8  where { noBytes _ = 1 }
instance ByteLength Word16 where { noBytes _ = 2 }
instance ByteLength Word32 where { noBytes _ = 4 }
instance ByteLength Word64 where { noBytes _ = 8 }

instance ByteLength BS.ByteString where
  noBytes = BS.length

-- * Partition a word into Word8s
-- ----------------------------------------------------------------------------

class ByteLength a => Bytes a where
  toBytes   :: a -> [Word8]
  fromBytes :: [Word8] -> a
  toByteString :: a -> BS.ByteString
  toByteString = BS.pack . toBytes -- default

instance Bytes Word8 where
  toBytes   = toBytesGen
  fromBytes = fromBytesGen

instance Bytes Word16 where
  toBytes   = toBytesGen
  fromBytes = fromBytesGen

instance Bytes Word32 where
  toBytes   = toBytesGen
  fromBytes = fromBytesGen

instance Bytes Word64 where
  toBytes   = toBytesGen
  fromBytes = fromBytesGen

instance Bytes BS.ByteString where
  toBytes   = BS.unpack
  fromBytes = BS.pack
  toByteString = id -- no unpacking and packing needed

-- * Utilities and generic implementations
-- ----------------------------------------------------------------------------

-- | Generic function for partitioning a word into Word8s
toBytesGen :: (Integral a, ByteLength a) => a -> [Word8]
toBytesGen num = unfoldr gen (noBytes num)
  where
    int = toInteger num

    gen :: Int -> Maybe (Word8,Int)
    gen i | i <= 0       = Nothing
          | k <- (i-1)*8 = Just (fromInteger $ (int .&. (0xFF `shiftL` k)) `shiftR` k, i-1)

-- | Generic function for obtaining a word from a list of Word8s
fromBytesGen :: (Num b, ByteLength b) => [Word8] -> b
fromBytesGen ws
  | length ws == noBytes result = result
  | otherwise = error "fromBytesGen" -- TODO: We need a representation of the type to print
  where result = fromInteger
               $ foldl' (\a w -> (a `shiftL` 8) .|. fromIntegral w) 0 ws

-- * A class for all types whose representation can be mesaured in words
-- ----------------------------------------------------------------------------

class WordLength a where
  noWords :: a -> Int

instance WordLength Word32 where { noWords _ = 1 }
instance WordLength Word64 where { noWords _ = 2 }

-- * Partition a word into Word32s
-- ----------------------------------------------------------------------------

class WordLength a => Words a where
  toWords   :: a -> [Word32]
  fromWords :: [Word32] -> a

instance Words Word32 where
  toWords   = toWordsGen
  fromWords = fromWordsGen

instance Words Word64 where
  toWords   = toWordsGen
  fromWords = fromWordsGen

-- * Utilities and generic implementations
-- ----------------------------------------------------------------------------

-- | Generic function for partitioning a word into Word32s
toWordsGen :: (Integral a, WordLength a) => a -> [Word32]
toWordsGen num = unfoldr gen (noWords num)
  where
    int = toInteger num

    gen :: Int -> Maybe (Word32,Int)
    gen i | i <= 0        = Nothing
          | k <- (i-1)*32 = Just (fromInteger $ (int .&. (0xFFFFFFFF `shiftL` k)) `shiftR` k, i-1)

-- | Generic function for obtaining a word from a list of Word32s
fromWordsGen :: (Num b, WordLength b) => [Word32] -> b
fromWordsGen ws
  | length ws == noWords result = result
  | otherwise = error "fromWordsGen" -- TODO: We need a representation of the type to print
  where result = fromInteger
               $ foldl' (\a w -> (a `shiftL` 32) .|. fromIntegral w) 0 ws

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
      pt1 = fromIntegral $ (w .&. 0xFF00) `shiftR` 8
      pt2 = fromIntegral $ (w .&. 0x00FF)
  fromHalves (w1,w2) = pt1 .|. pt2
    where
      pt1 = fromIntegral w1 `shiftL` 8
      pt2 = fromIntegral w2

instance Split Word32 where
  type Half Word32 = Word16
  toHalves w = (pt1,pt2)
    where
      pt1 = fromIntegral $ (w .&. 0xFFFF0000) `shiftR` 16
      pt2 = fromIntegral $ (w .&. 0x0000FFFF)
  fromHalves (w1,w2) = pt1 .|. pt2
    where
      pt1 = fromIntegral w1 `shiftL` 16
      pt2 = fromIntegral w2

instance Split Word64 where
  type Half Word64 = Word32
  toHalves w = (pt1,pt2)
    where
      pt1 = fromIntegral $ (w .&. 0xFFFFFFFF00000000) `shiftR` 32
      pt2 = fromIntegral $ (w .&. 0x00000000FFFFFFFF)
  fromHalves (w1,w2) = pt1 .|. pt2
    where
      pt1 = fromIntegral w1 `shiftL` 32
      pt2 = fromIntegral w2


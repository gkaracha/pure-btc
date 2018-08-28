{-# OPTIONS_GHC -Wall #-}

module Utils.Bytes
( ByteLength(..)
, Bytes(..)
, toBytesGen
, fromBytesGen
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


{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeFamilies #-}

-- Re-export all word types
module Words
( module Data.Word
, module Data.Bits
, Word128, Word256, Word512
, w128ToInteger, w256ToInteger, w512ToInteger -- TODO: No 'Integral' instances for these.
, Split(..), Bytes(..), Words(..)
) where

import Data.Word
import Data.Bits
import qualified Data.ByteString as BS
import Data.List (foldl')
import Word128
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

instance Bytes Word8 where
  toBytes w = [w]
  fromBytes = fromBytesGen

instance Bytes Word16 where
  toBytes w = [w1,w2]
    where (w1,w2) = toHalves w
  fromBytes = fromBytesGen

instance Bytes Word32 where
  toBytes w = toBytes w1 ++ toBytes w2
    where (w1,w2) = toHalves w
  fromBytes = fromBytesGen

instance Bytes Word64 where
  toBytes w = toBytes w1 ++ toBytes w2
    where (w1,w2) = toHalves w
  fromBytes = fromBytesGen

instance Bytes Word128 where
  toBytes = w128ToBytes
  fromBytes = fromBytesGen

instance Bytes Word256 where
  toBytes = w256ToBytes
  fromBytes = fromBytesGen

instance Bytes Word512 where
  toBytes = w512ToBytes
  fromBytes = fromBytesGen

instance Bytes BS.ByteString where
  toBytes   = BS.unpack
  fromBytes = BS.pack

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

instance Words Word32  -- TODO
instance Words Word64  -- TODO
instance Words Word128 -- TODO
instance Words Word256 -- TODO
instance Words Word512 -- TODO


-- || -- * Parse ByteStrings as words
-- || -- ----------------------------------------------------------------------------
-- ||
-- || bs_w8 :: BS.ByteString -> Word8
-- || bs_w8 = fromBytes . toBytes
-- ||
-- || bs_w16 :: BS.ByteString -> Word16
-- || bs_w16 = fromBytes . toBytes
-- ||
-- || bs_w32 :: BS.ByteString -> Word32
-- || bs_w32 = fromBytes . toBytes
-- ||
-- || bs_w64 :: BS.ByteString -> Word64
-- || bs_w64 = fromBytes . toBytes
-- ||
-- || -- TODO: bs_w128 :: BS.ByteString -> Word128
-- || -- TODO: bs_w256 :: BS.ByteString -> Word256
-- || -- TODO: bs_w512 :: BS.ByteString -> Word512


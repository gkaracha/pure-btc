{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeFamilies #-}

module Word256 (Word256, w256ToW128s, w128sToW256, w256ToBytes, w256ToInteger) where

import Data.Word
import Data.Bits
import Data.Function (on)
import Data.Ratio ((%))
import Word128

-- * Word256, masks, and utilities
-- ----------------------------------------------------------------------------

newtype Word256 = W256 { w256 :: Integer }
-- TODO: representation as an Integer is not the most efficient but at least
-- keeps things simple. For now.

mask256 :: Integer
mask256 = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF

mask256L :: Integer
mask256L = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000000000000000000000

mask256R :: Integer
mask256R = 0x00000000000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF

-- | Cutoff for Word256
narrow256Integer :: Integer -> Integer
narrow256Integer i = i .&. mask256

-- * Basic Instances
-- ----------------------------------------------------------------------------

instance Show Word256 where
  show (W256 i) = show i

instance Eq Word256 where
  (==) = (==) `on` w256

instance Ord Word256 where
  compare = compare `on` w256

instance Bounded Word256 where
  minBound = W256 0x0000000000000000000000000000000000000000000000000000000000000000
  maxBound = W256 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF

instance Bits Word256 where
  W256 i1  .&.  W256 i2 = W256 (i1  .&.  i2)
  W256 i1  .|.  W256 i2 = W256 (i1  .|.  i2)
  W256 i1 `xor` W256 i2 = W256 (i1 `xor` i2)
  complement w = w `xor` maxBound
  zeroBits = minBound
  bit n | n < 0 || n > 255 = minBound
        | otherwise        = W256 (1 `shiftL` n)
  testBit (W256 i) n = testBit i n
  bitSizeMaybe _ = Just 256
  bitSize _ = 256 -- TODO: Use the FiniteBits method instead
  isSigned _ = False
  shiftL (W256 i) n = W256 $ (i `shiftL` n) .&. mask256
  shiftR (W256 i) n = W256 $ i `shiftR` n -- no need to mask this
  rotateL w n = shiftL w m .|. shiftR w (256 - m) where m = n `mod` 256
  rotateR w n = shiftR w m .|. shiftL w (256 - m) where m = n `mod` 256
  popCount (W256 i) = popCount i

instance FiniteBits Word256 where
  finiteBitSize _ = 256

instance Num Word256 where
  W256 i1 + W256 i2 = W256 $ (i1 + i2) .&. mask256
  W256 i1 * W256 i2 = W256 $ (i1 * i2) .&. mask256
  negate w = complement w + W256 1 -- 2-Complement (TODO: Double-check this)
  abs w = w
  signum (W256 0) = W256 0
  signum (W256 _) = W256 1
  fromInteger = W256 . narrow256Integer

instance Real Word256 where
  toRational (W256 i) = i % 1

-- * Specialized Instances
-- ----------------------------------------------------------------------------

w256ToW128s :: Word256 -> (Word128, Word128)
w256ToW128s (W256 i) = (pt1,pt2)
  where
    pt1 = fromInteger $ (i .&. mask256L) `rotateR` 128
    pt2 = fromInteger $ (i .&. mask256R)

w128sToW256 :: (Word128, Word128) -> Word256
w128sToW256 (w1,w2) = W256 (pt1 .|. pt2)
  where
    pt1 = w128ToInteger w1 `rotateL` 128
    pt2 = w128ToInteger w2

w256ToBytes :: Word256 -> [Word8]
w256ToBytes w = w128ToBytes w1 ++ w128ToBytes w2
  where
    (w1,w2) = w256ToW128s w

-- | instance Bytes Word256 where
-- |   toBytes w = toBytes w1 ++ toBytes w2
-- |     where (w1,w2) = split w
-- |   fromBytes ws = W256 $ narrow256Integer $ fromBytesGen ws

w256ToInteger :: Word256 -> Integer
w256ToInteger = w256


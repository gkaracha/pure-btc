{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeFamilies #-}

module Word128 (Word128) where

import Data.Word
import Data.Bits
import Convert
import Data.Function (on)
import Data.Ratio ((%))

-- * Word128, masks, and utilities
-- ----------------------------------------------------------------------------

newtype Word128 = W128 { w128 :: Integer }
-- TODO: representation as an Integer is not the most efficient but at least
-- keeps things simple. For now.

mask128 :: Integer
mask128 = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF

mask128L :: Integer
mask128L = 0xFFFFFFFFFFFFFFFF0000000000000000

mask128R :: Integer
mask128R = 0x0000000000000000FFFFFFFFFFFFFFFF

narrow128Integer :: Integer -> Integer
narrow128Integer i = i .&. mask128

-- * Basic Instances
-- ----------------------------------------------------------------------------

instance Show Word128 where
  show (W128 i) = show i

instance Eq Word128 where
  (==) = (==) `on` w128

instance Ord Word128 where
  compare = compare `on` w128

instance Bounded Word128 where
  minBound = W128 0x00000000000000000000000000000000
  maxBound = W128 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF

instance Bits Word128 where
  W128 i1  .&.  W128 i2 = W128 (i1  .&.  i2)
  W128 i1  .|.  W128 i2 = W128 (i1  .|.  i2)
  W128 i1 `xor` W128 i2 = W128 (i1 `xor` i2)
  complement w = w `xor` maxBound
  zeroBits = minBound
  bit n | n < 0 || n > 127 = minBound
        | otherwise        = W128 (1 `shiftL` n)
  testBit (W128 i) n = testBit i n
  bitSizeMaybe _ = Just 128
  bitSize _ = 128 -- TODO: Use the FiniteBits method instead
  isSigned _ = False
  shiftL (W128 i) n = W128 $ (i `shiftL` n) .&. mask128
  shiftR (W128 i) n = W128 $ i `shiftR` n -- no need to mask this
  rotateL w n = shiftL w m .|. shiftR w (128 - m) where m = n `mod` 128
  rotateR w n = shiftR w m .|. shiftL w (128 - m) where m = n `mod` 128
  popCount (W128 i) = popCount i

instance FiniteBits Word128 where
  finiteBitSize _ = 128

instance Num Word128 where
  W128 i1 + W128 i2 = W128 $ (i1 + i2) .&. mask128
  W128 i1 * W128 i2 = W128 $ (i1 * i2) .&. mask128
  negate w = complement w + W128 1 -- 2-Complement (TODO: Double-check this)
  abs w = w
  signum (W128 0) = W128 0
  signum (W128 _) = W128 1
  fromInteger = W128 . narrow128Integer

instance Real Word128 where
  toRational (W128 i) = i % 1

-- * Specialized Instances
-- ----------------------------------------------------------------------------

instance Split Word128 where
  type Half Word128 = Word64
  split (W128 i) = (pt1,pt2)
    where
      pt1 = fromIntegral $ (i .&. mask128L) `rotateR` 64
      pt2 = fromIntegral $ (i .&. mask128R)

instance Merge Word64 where
  type Twice Word64 = Word128
  merge = W128 . mergeGen

instance Bytes Word128 where
  toBytes w = toBytes w1 ++ toBytes w2
    where (w1,w2) = split w
  fromBytes ws = W128 $ narrow128Integer $ fromBytesGen ws


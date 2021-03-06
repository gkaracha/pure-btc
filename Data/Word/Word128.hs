{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Word.Word128
-- Copyright   :  (c) Georgios Karachalias, 2018
-- License     :  BSD3
--
-- Maintainer  :  gdkaracha@gmail.com
-- Stability   :  experimental
-- Portability :  GHC
--
-- Unsigned integral type with a size of 128 bits.
--
-----------------------------------------------------------------------------

module Data.Word.Word128 (Word128) where

import Data.Word
import Data.Bits
import Data.Function (on)
import Data.Ratio ((%))
import Util.Error (toEnumError, fromEnumError)
import Data.Word.Partition

-- * Word128, masks, and utilities
-- ----------------------------------------------------------------------------

-- | A 'Word128' is an unsigned integral type, with a size of 128 bits.
newtype Word128 = W128 { w128 :: Integer }

mask128 :: Integer
mask128 = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF

mask128L :: Integer
mask128L = 0xFFFFFFFFFFFFFFFF0000000000000000

mask128R :: Integer
mask128R = 0x0000000000000000FFFFFFFFFFFFFFFF

-- | Cutoff for Word128
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
  minBound = 0x00000000000000000000000000000000
  maxBound = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF

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

instance Enum Word128 where
  toEnum int
    | int >= 0  = W128 (fromIntegral int) -- no need to mask it, it's small
    | otherwise = toEnumError "Word128" int (minBound :: Word128, maxBound :: Word128)
  fromEnum w@(W128 i)
    | i <= fromIntegral (maxBound :: Int) = fromIntegral i
    | otherwise                           = fromEnumError "Word128" w

instance Integral Word128 where
  quotRem (W128 i1) (W128 i2) = (W128 q, W128 r) -- no need to mask them
    where (q,r) = quotRem i1 i2
  toInteger (W128 i) = i

-- * Specialized Instances
-- ----------------------------------------------------------------------------

instance Split Word128 where
  type Half Word128 = Word64
  toHalves   = w128ToW64s
  fromHalves = w64sToW128

-- | Split a 'Word128' into two 'Word64's
w128ToW64s :: Word128 -> (Word64, Word64)
w128ToW64s (W128 i) = (pt1,pt2)
  where
    pt1 = fromIntegral $ (i .&. mask128L) `rotateR` 64
    pt2 = fromIntegral $ (i .&. mask128R)

-- | Combine two 'Word64's into a 'Word128'
w64sToW128 :: (Word64, Word64) -> Word128
w64sToW128 (w1,w2) = W128 (pt1 .|. pt2)
  where
    pt1 = fromIntegral w1 `rotateL` 64
    pt2 = fromIntegral w2

instance ByteLength Word128 where
  noBytes _ = 16

instance Bytes Word128 where
  toBytes   = toBytesGen
  fromBytes = fromBytesGen

instance WordLength Word128 where
  noWords _ = 4

instance Words Word128 where
  toWords   = toWordsGen
  fromWords = fromWordsGen


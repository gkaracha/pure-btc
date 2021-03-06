{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Word.Word256
-- Copyright   :  (c) Georgios Karachalias, 2018
-- License     :  BSD3
--
-- Maintainer  :  gdkaracha@gmail.com
-- Stability   :  experimental
-- Portability :  GHC
--
-- Unsigned integral type with a size of 256 bits.
--
-----------------------------------------------------------------------------

module Data.Word.Word256 (Word256) where

import Data.Bits
import Data.Function (on)
import Data.Ratio ((%))
import Util.Error (toEnumError, fromEnumError)
import Data.Word.Word128 (Word128)
import Data.Word.Partition

-- * Word256, masks, and utilities
-- ----------------------------------------------------------------------------

-- | A 'Word256' is an unsigned integral type, with a size of 256 bits.
newtype Word256 = W256 { w256 :: Integer }

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

instance Enum Word256 where
  toEnum int
    | int >= 0  = W256 (fromIntegral int) -- no need to mask it, it's small
    | otherwise = toEnumError "Word256" int (minBound :: Word256, maxBound :: Word256)
  fromEnum w@(W256 i)
    | i <= fromIntegral (maxBound :: Int) = fromIntegral i
    | otherwise                           = fromEnumError "Word256" w

instance Integral Word256 where
  quotRem (W256 i1) (W256 i2) = (W256 q, W256 r) -- no need to mask them
    where (q,r) = quotRem i1 i2
  toInteger (W256 i) = i

-- * Specialized Instances
-- ----------------------------------------------------------------------------

instance Split Word256 where
  type Half Word256 = Word128
  toHalves   = w256ToW128s
  fromHalves = w128sToW256

-- | Split a 'Word256' into two 'Word128's
w256ToW128s :: Word256 -> (Word128, Word128)
w256ToW128s (W256 i) = (pt1,pt2)
  where
    pt1 = fromInteger $ (i .&. mask256L) `rotateR` 128
    pt2 = fromInteger $ (i .&. mask256R)

-- | Combine two 'Word128's into a 'Word256'
w128sToW256 :: (Word128, Word128) -> Word256
w128sToW256 (w1,w2) = W256 (pt1 .|. pt2)
  where
    pt1 = fromIntegral w1 `rotateL` 128
    pt2 = fromIntegral w2

instance ByteLength Word256 where
  noBytes _ = 32

instance Bytes Word256 where
  toBytes   = toBytesGen
  fromBytes = fromBytesGen

instance WordLength Word256 where
  noWords _ = 8

instance Words Word256 where
  toWords   = toWordsGen
  fromWords = fromWordsGen


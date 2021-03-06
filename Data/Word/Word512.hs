{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Word.Word512
-- Copyright   :  (c) Georgios Karachalias, 2018
-- License     :  BSD3
--
-- Maintainer  :  gdkaracha@gmail.com
-- Stability   :  experimental
-- Portability :  GHC
--
-- Unsigned integral type with a size of 512 bits.
--
-----------------------------------------------------------------------------

module Data.Word.Word512 (Word512) where

import Data.Bits
import Data.Function (on)
import Data.Ratio ((%))
import Util.Error (toEnumError, fromEnumError)
import Data.Word.Word256 (Word256)
import Data.Word.Partition

-- * Word512, masks, and utilities
-- ----------------------------------------------------------------------------

-- | A 'Word512' is an unsigned integral type, with a size of 512 bits.
newtype Word512 = W512 { w512 :: Integer }

mask512 :: Integer
mask512 = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF

mask512L :: Integer
mask512L = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000000000000000000000000000000000000000000000000000000000000000

mask512R :: Integer
mask512R = 0x0000000000000000000000000000000000000000000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF

-- | Cutoff for Word512
narrow512Integer :: Integer -> Integer
narrow512Integer i = i .&. mask512

-- * Basic Instances
-- ----------------------------------------------------------------------------

instance Show Word512 where
  show (W512 i) = show i

instance Eq Word512 where
  (==) = (==) `on` w512

instance Ord Word512 where
  compare = compare `on` w512

instance Bounded Word512 where
  minBound = W512 0x00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
  maxBound = W512 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF

instance Bits Word512 where
  W512 i1  .&.  W512 i2 = W512 (i1  .&.  i2)
  W512 i1  .|.  W512 i2 = W512 (i1  .|.  i2)
  W512 i1 `xor` W512 i2 = W512 (i1 `xor` i2)
  complement w = w `xor` maxBound
  zeroBits = minBound
  bit n | n < 0 || n > 511 = minBound
        | otherwise        = W512 (1 `shiftL` n)
  testBit (W512 i) n = testBit i n
  bitSizeMaybe _ = Just 512
  bitSize _ = 512 -- TODO: Use the FiniteBits method instead
  isSigned _ = False
  shiftL (W512 i) n = W512 $ (i `shiftL` n) .&. mask512
  shiftR (W512 i) n = W512 $ i `shiftR` n -- no need to mask this
  rotateL w n = shiftL w m .|. shiftR w (512 - m) where m = n `mod` 512
  rotateR w n = shiftR w m .|. shiftL w (512 - m) where m = n `mod` 512
  popCount (W512 i) = popCount i

instance FiniteBits Word512 where
  finiteBitSize _ = 512

instance Num Word512 where
  W512 i1 + W512 i2 = W512 $ (i1 + i2) .&. mask512
  W512 i1 * W512 i2 = W512 $ (i1 * i2) .&. mask512
  negate w = complement w + W512 1 -- 2-Complement (TODO: Double-check this)
  abs w = w
  signum (W512 0) = W512 0
  signum (W512 _) = W512 1
  fromInteger = W512 . narrow512Integer

instance Real Word512 where
  toRational (W512 i) = i % 1

instance Enum Word512 where
  toEnum int
    | int >= 0  = W512 (fromIntegral int) -- no need to mask it, it's small
    | otherwise = toEnumError "Word512" int (minBound :: Word512, maxBound :: Word512)
  fromEnum w@(W512 i)
    | i <= fromIntegral (maxBound :: Int) = fromIntegral i
    | otherwise                           = fromEnumError "Word512" w

instance Integral Word512 where
  quotRem (W512 i1) (W512 i2) = (W512 q, W512 r) -- no need to mask them
    where (q,r) = quotRem i1 i2
  toInteger (W512 i) = i

-- * Specialized Instances
-- ----------------------------------------------------------------------------

instance Split Word512 where
  type Half Word512 = Word256
  toHalves   = w512ToW256s
  fromHalves = w256sToW512

-- | Split a 'Word512' into two 'Word256's
w512ToW256s :: Word512 -> (Word256, Word256)
w512ToW256s (W512 i) = (pt1,pt2)
  where
    pt1 = fromInteger $ (i .&. mask512L) `rotateR` 256
    pt2 = fromInteger $ (i .&. mask512R)

-- | Combine two 'Word256's into a 'Word512'
w256sToW512 :: (Word256, Word256) -> Word512
w256sToW512 (w1,w2) = W512 (pt1 .|. pt2)
  where
    pt1 = fromIntegral w1 `rotateL` 256
    pt2 = fromIntegral w2

-- TODO: Change some rotates to shifts?

instance ByteLength Word512 where
  noBytes _ = 64

instance Bytes Word512 where
  toBytes   = toBytesGen
  fromBytes = fromBytesGen

instance WordLength Word512 where
  noWords _ = 16

instance Words Word512 where
  toWords   = toWordsGen
  fromWords = fromWordsGen


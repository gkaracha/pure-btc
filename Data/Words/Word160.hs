{-# OPTIONS_GHC -Wall #-}

module Data.Words.Word160 (Word160) where

import Data.Bits
import Data.Function (on)
import Data.Ratio ((%))
import Utils.Error (toEnumError, fromEnumError)
import Data.Words.Partition

-- 160 bits == 20 bytes == 40 hex digits == 5 Word32s

-- * Word160, masks, and utilities
-- ----------------------------------------------------------------------------

newtype Word160 = W160 { w160 :: Integer }
-- TODO: representation as an Integer is not the most efficient but at least
-- keeps things simple. For now.

mask160 :: Integer
mask160 = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF

-- | Cutoff for Word160
narrow160Integer :: Integer -> Integer
narrow160Integer i = i .&. mask160

-- * Basic Instances
-- ----------------------------------------------------------------------------

instance Show Word160 where
  show (W160 i) = show i

instance Eq Word160 where
  (==) = (==) `on` w160

instance Ord Word160 where
  compare = compare `on` w160

instance Bounded Word160 where
  minBound = W160 0x0000000000000000000000000000000000000000
  maxBound = W160 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF

instance Bits Word160 where
  W160 i1  .&.  W160 i2 = W160 (i1  .&.  i2)
  W160 i1  .|.  W160 i2 = W160 (i1  .|.  i2)
  W160 i1 `xor` W160 i2 = W160 (i1 `xor` i2)
  complement w = w `xor` maxBound
  zeroBits = minBound
  bit n | n < 0 || n > 159 = minBound
        | otherwise        = W160 (1 `shiftL` n)
  testBit (W160 i) n = testBit i n
  bitSizeMaybe _ = Just 160
  bitSize _ = 160 -- TODO: Use the FiniteBits method instead
  isSigned _ = False
  shiftL (W160 i) n = W160 $ (i `shiftL` n) .&. mask160
  shiftR (W160 i) n = W160 $ i `shiftR` n -- no need to mask this
  rotateL w n = shiftL w m .|. shiftR w (160 - m) where m = n `mod` 160
  rotateR w n = shiftR w m .|. shiftL w (160 - m) where m = n `mod` 160
  popCount (W160 i) = popCount i

instance FiniteBits Word160 where
  finiteBitSize _ = 160

instance Num Word160 where
  W160 i1 + W160 i2 = W160 $ (i1 + i2) .&. mask160
  W160 i1 * W160 i2 = W160 $ (i1 * i2) .&. mask160
  negate w = complement w + W160 1 -- 2-Complement (TODO: Double-check this)
  abs w = w
  signum (W160 0) = W160 0
  signum (W160 _) = W160 1
  fromInteger = W160 . narrow160Integer

instance Real Word160 where
  toRational (W160 i) = i % 1

instance Enum Word160 where
  toEnum int
    | int >= 0  = W160 (fromIntegral int) -- no need to mask it, it's small
    | otherwise = toEnumError "Word160" int (minBound :: Word160, maxBound :: Word160)
  fromEnum w@(W160 i)
    | i <= fromIntegral (maxBound :: Int) = fromIntegral i
    | otherwise                           = fromEnumError "Word160" w

instance Integral Word160 where
  quotRem (W160 i1) (W160 i2) = (W160 q, W160 r) -- no need to mask them
    where (q,r) = quotRem i1 i2
  toInteger (W160 i) = i

-- * Specialized Instances
-- ----------------------------------------------------------------------------

instance ByteLength Word160 where
  noBytes _ = 20

instance Bytes Word160 where
  toBytes   = toBytesGen
  fromBytes = fromBytesGen

instance WordLength Word160 where
  noWords _ = 5

instance Words Word160 where
  toWords   = toWordsGen
  fromWords = fromWordsGen


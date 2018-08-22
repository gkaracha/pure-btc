{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Words.Word160 (Word160, w160ToWords, w160ToBytes) where

import Data.Word
import Data.Bits
import Data.Function (on)
import Data.Ratio ((%))
import Utils.Utils

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

w160ToWords :: Word160 -> [Word32]
w160ToWords (W160 i)
  = [ fromIntegral $ (i .&. 0xFFFFFFFF00000000000000000000000000000000) `shiftR` 128
    , fromIntegral $ (i .&. 0x00000000FFFFFFFF000000000000000000000000) `shiftR` 96
    , fromIntegral $ (i .&. 0x0000000000000000FFFFFFFF0000000000000000) `shiftR` 64
    , fromIntegral $ (i .&. 0x000000000000000000000000FFFFFFFF00000000) `shiftR` 32
    , fromIntegral $ (i .&. 0x00000000000000000000000000000000FFFFFFFF) ]

w160ToBytes :: Word160 -> [Word8]
w160ToBytes (W160 i)
  = [ fromIntegral $ (i .&. 0xFF00000000000000000000000000000000000000) `shiftR` 152
    , fromIntegral $ (i .&. 0x00FF000000000000000000000000000000000000) `shiftR` 144
    , fromIntegral $ (i .&. 0x0000FF0000000000000000000000000000000000) `shiftR` 136
    , fromIntegral $ (i .&. 0x000000FF00000000000000000000000000000000) `shiftR` 128
    , fromIntegral $ (i .&. 0x00000000FF000000000000000000000000000000) `shiftR` 120
    , fromIntegral $ (i .&. 0x0000000000FF0000000000000000000000000000) `shiftR` 112
    , fromIntegral $ (i .&. 0x000000000000FF00000000000000000000000000) `shiftR` 104
    , fromIntegral $ (i .&. 0x00000000000000FF000000000000000000000000) `shiftR` 96
    , fromIntegral $ (i .&. 0x0000000000000000FF0000000000000000000000) `shiftR` 88
    , fromIntegral $ (i .&. 0x000000000000000000FF00000000000000000000) `shiftR` 80
    , fromIntegral $ (i .&. 0x00000000000000000000FF000000000000000000) `shiftR` 72
    , fromIntegral $ (i .&. 0x0000000000000000000000FF0000000000000000) `shiftR` 64
    , fromIntegral $ (i .&. 0x000000000000000000000000FF00000000000000) `shiftR` 56
    , fromIntegral $ (i .&. 0x00000000000000000000000000FF000000000000) `shiftR` 48
    , fromIntegral $ (i .&. 0x0000000000000000000000000000FF0000000000) `shiftR` 40
    , fromIntegral $ (i .&. 0x000000000000000000000000000000FF00000000) `shiftR` 32
    , fromIntegral $ (i .&. 0x00000000000000000000000000000000FF000000) `shiftR` 24
    , fromIntegral $ (i .&. 0x0000000000000000000000000000000000FF0000) `shiftR` 16
    , fromIntegral $ (i .&. 0x000000000000000000000000000000000000FF00) `shiftR` 8
    , fromIntegral $ (i .&. 0x00000000000000000000000000000000000000FF) ]


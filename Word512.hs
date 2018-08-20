{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeFamilies #-}

module Word512 (Word512, w512ToW256s, w256sToW512, w512ToBytes, w512ToInteger) where

import Data.Word
import Data.Bits
import Data.Function (on)
import Data.Ratio ((%))
import Word256

-- * Word512, masks, and utilities
-- ----------------------------------------------------------------------------

newtype Word512 = W512 { w512 :: Integer }
-- TODO: representation as an Integer is not the most efficient but at least
-- keeps things simple. For now.

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

-- * Specialized Instances
-- ----------------------------------------------------------------------------

w512ToW256s :: Word512 -> (Word256, Word256)
w512ToW256s (W512 i) = (pt1,pt2)
  where
    pt1 = fromInteger $ (i .&. mask512L) `rotateR` 256
    pt2 = fromInteger $ (i .&. mask512R)

w256sToW512 :: (Word256, Word256) -> Word512
w256sToW512 (w1,w2) = W512 (pt1 .|. pt2)
  where
    pt1 = w256ToInteger w1 `rotateL` 256
    pt2 = w256ToInteger w2

w512ToBytes :: Word512 -> [Word8]
w512ToBytes w = w256ToBytes w1 ++ w256ToBytes w2
  where
    (w1,w2) = w512ToW256s w

-- TODO: Change some rotates to shifts?

w512ToInteger :: Word512 -> Integer
w512ToInteger = w512


{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeFamilies #-}

module Utils.Split
( Split(..)

) where

import Data.Word
import Data.Bits (shiftL, shiftR, (.|.), (.&.))

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
      pt1 = fromIntegral $ (w .&. 0xFF00) `shiftR` 8
      pt2 = fromIntegral $ (w .&. 0x00FF)
  fromHalves (w1,w2) = pt1 .|. pt2
    where
      pt1 = fromIntegral w1 `shiftL` 8
      pt2 = fromIntegral w2

instance Split Word32 where
  type Half Word32 = Word16
  toHalves w = (pt1,pt2)
    where
      pt1 = fromIntegral $ (w .&. 0xFFFF0000) `shiftR` 16
      pt2 = fromIntegral $ (w .&. 0x0000FFFF)
  fromHalves (w1,w2) = pt1 .|. pt2
    where
      pt1 = fromIntegral w1 `shiftL` 16
      pt2 = fromIntegral w2

instance Split Word64 where
  type Half Word64 = Word32
  toHalves w = (pt1,pt2)
    where
      pt1 = fromIntegral $ (w .&. 0xFFFFFFFF00000000) `shiftR` 32
      pt2 = fromIntegral $ (w .&. 0x00000000FFFFFFFF)
  fromHalves (w1,w2) = pt1 .|. pt2
    where
      pt1 = fromIntegral w1 `shiftL` 32
      pt2 = fromIntegral w2


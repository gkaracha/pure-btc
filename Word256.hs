{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeFamilies #-}

module Word256 (Word256, iToWord256) where

-- import Data.Word
import Data.Bits
import Convert
import Data.Function (on)
import Word128

newtype Word256 = W256 { w256 :: Integer }
-- TODO: representation as an Integer is not the most efficient but at least
-- keeps things simple. For now.

iToWord256 :: Integer -> Word256
iToWord256 = W256 . narrow256Integer
-- TODO: We actually need 'fromInteger' but I am too bored to give the instances now

-- | Cutoff for Word256
narrow256Integer :: Integer -> Integer
narrow256Integer i = i .&. 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF

instance Show Word256 where
  show (W256 i) = show i

instance Eq Word256 where
  (==) = (==) `on` w256

instance Ord Word256 where
  compare = compare `on` w256

instance Split Word256 where
  type Half Word256 = Word128
  split (W256 i) = (pt1,pt2)
    where
      pt1 = iToWord128
          $ (i .&. 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000000000000000000000) `rotateR` 128
      pt2 = iToWord128
          $ (i .&. 0x00000000000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)

instance Merge Word128 where
  type Twice Word128 = Word256
-- || mergeGen :: (Num b, Bits b, FiniteBits a, Integral a) => (a,a) -> b
-- || mergeGen (w1,w2) = pt1 .|. pt2
-- ||   where
-- ||     pt1 = fromIntegral w1 `rotateL` finiteBitSize w1
-- ||     pt2 = fromIntegral w2

instance Bytes Word256 where
  toBytes w = toBytes w1 ++ toBytes w2
    where (w1,w2) = split w
  fromBytes ws = W256 $ narrow256Integer $ fromBytesGen ws










-- SUPERCLASS GRAPH
--
-- || class Num a where
-- || class Eq a where
-- || class Eq a => Ord a where
-- || class (Num a, Ord a) => Real a where
-- || class Enum a where
-- || class (Real a, Enum a) => Integral a where
-- || class Eq a => Bits a where
-- || class Bits b => FiniteBits b where
-- DON'T FORGET TO SHOW IT!


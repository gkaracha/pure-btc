{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeFamilies #-}

module Word128 (Word128, iToWord128) where

import Data.Word
import Data.Bits
import Convert
import Data.Function (on)

newtype Word128 = W128 { w128 :: Integer }
-- TODO: representation as an Integer is not the most efficient but at least
-- keeps things simple. For now.

iToWord128 :: Integer -> Word128
iToWord128 = W128 . narrow128Integer
-- TODO: We actually need 'fromInteger' but I am too bored to give the instances now

-- | Cutoff for Word128
narrow128Integer :: Integer -> Integer
narrow128Integer i = i .&. 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF

instance Show Word128 where
  show (W128 i) = show i

instance Eq Word128 where
  (==) = (==) `on` w128

instance Ord Word128 where
  compare = compare `on` w128

instance Split Word128 where
  type Half Word128 = Word64
  split (W128 i) = (pt1,pt2)
    where
      pt1 = fromIntegral $ (i .&. 0xFFFFFFFFFFFFFFFF0000000000000000) `rotateR` 64
      pt2 = fromIntegral $ (i .&. 0x0000000000000000FFFFFFFFFFFFFFFF)

instance Merge Word64 where
  type Twice Word64 = Word128
  merge = W128 . mergeGen

instance Bytes Word128 where
  toBytes w = toBytes w1 ++ toBytes w2
    where (w1,w2) = split w
  fromBytes ws = W128 $ narrow128Integer $ fromBytesGen ws










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


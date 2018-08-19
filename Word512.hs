{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeFamilies #-}

module Word512 (Word512, iToWord512) where

-- import Data.Word
import Data.Bits
import Convert
import Data.Function (on)
import Word256

newtype Word512 = W512 { w512 :: Integer }
-- TODO: representation as an Integer is not the most efficient but at least
-- keeps things simple. For now.

iToWord512 :: Integer -> Word512
iToWord512 = W512 . narrow512Integer
-- TODO: We actually need 'fromInteger' but I am too bored to give the instances now

-- | Cutoff for Word512
narrow512Integer :: Integer -> Integer
narrow512Integer i = i .&. 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF

instance Show Word512 where
  show (W512 i) = show i

instance Eq Word512 where
  (==) = (==) `on` w512

instance Ord Word512 where
  compare = compare `on` w512

instance Split Word512 where
  type Half Word512 = Word256
  split (W512 i) = (pt1,pt2)
    where
      pt1 = iToWord256
          $ (i .&. 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000000000000000000000000000000000000000000000000000000000000000) `rotateR` 256
      pt2 = iToWord256
          $ (i .&. 0x0000000000000000000000000000000000000000000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)

instance Merge Word256 where
  type Twice Word256 = Word512
-- || mergeGen :: (Num b, Bits b, FiniteBits a, Integral a) => (a,a) -> b
-- || mergeGen (w1,w2) = pt1 .|. pt2
-- ||   where
-- ||     pt1 = fromIntegral w1 `rotateL` finiteBitSize w1
-- ||     pt2 = fromIntegral w2

instance Bytes Word512 where
  toBytes w = toBytes w1 ++ toBytes w2
    where (w1,w2) = split w
  fromBytes ws = W512 $ narrow512Integer $ fromBytesGen ws










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


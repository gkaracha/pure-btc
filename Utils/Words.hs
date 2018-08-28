{-# OPTIONS_GHC -Wall #-}

module Utils.Words
( WordLength(..)
, Words(..)
, toWordsGen
, fromWordsGen
) where

import Data.Word (Word32, Word64)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.List (unfoldr, foldl')

-- * A class for all types whose representation can be mesaured in words
-- ----------------------------------------------------------------------------

class WordLength a where
  noWords :: a -> Int

instance WordLength Word32 where { noWords _ = 1 }
instance WordLength Word64 where { noWords _ = 2 }

-- * Partition a word into Word32s
-- ----------------------------------------------------------------------------

class WordLength a => Words a where
  toWords   :: a -> [Word32]
  fromWords :: [Word32] -> a

instance Words Word32 where
  toWords   = toWordsGen
  fromWords = fromWordsGen

instance Words Word64 where
  toWords   = toWordsGen
  fromWords = fromWordsGen

-- * Utilities and generic implementations
-- ----------------------------------------------------------------------------

-- | Generic function for partitioning a word into Word32s
toWordsGen :: (Integral a, WordLength a) => a -> [Word32]
toWordsGen num = unfoldr gen (noWords num)
  where
    int = toInteger num

    gen :: Int -> Maybe (Word32,Int)
    gen i | i <= 0        = Nothing
          | k <- (i-1)*32 = Just (fromInteger $ (int .&. (0xFFFFFFFF `shiftL` k)) `shiftR` k, i-1)

-- | Generic function for obtaining a word from a list of Word32s
fromWordsGen :: (Num b, WordLength b) => [Word32] -> b
fromWordsGen ws
  | length ws == noWords result = result
  | otherwise = error "fromWordsGen" -- TODO: We need a representation of the type to print
  where result = fromInteger
               $ foldl' (\a w -> (a `shiftL` 32) .|. fromIntegral w) 0 ws


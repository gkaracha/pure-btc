{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}

module Util.Numeric
( integerSqrt
, modPow
) where

import Data.Bits (shiftL, shiftR)

-- * Efficient operations on Integers
-- ----------------------------------------------------------------------------

integerSqrt :: Integer -> Integer
integerSqrt n
  | n < 0 = error $ "integerSqrt: negative input " ++ show n
  | n < 2 = n
  | otherwise = let small = integerSqrt(n `shiftR` 2) `shiftL` 1 in
                let large = small + 1                            in
                if large * large > n then small else large

modPow :: Integer -> Integer -> Integer -> Integer
modPow base ex modulo = modPow' (base `mod` modulo) ex modulo 1
  where
    modPow' _b 0 _m !r = r
    modPow'  b e  m  r = modPow' (b * b `mod` m) (e `div` 2) m
                                 (if even e then r else (r * b `mod` m))


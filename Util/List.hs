{-# OPTIONS_GHC -Wall #-}

module Util.List
( listChunksOf
, pairs
, splitInTwo
, splitInThree
, splitInMany -- TODO: not sure if we are gonna need this one
) where

import Data.List (unfoldr)
import Control.Monad (guard)

listChunksOf :: Int -> [a] -> [[a]]
listChunksOf x = unfoldr gen
  where
    gen a | null a    = Nothing
          | otherwise = Just (splitAt x a)

pairs :: [a] -> Maybe [(a,a)]
pairs []       = Just []
pairs (x:y:ys) = fmap ((x,y):) (pairs ys)
pairs (_:_)    = Nothing

splitInTwo :: Int -> Int -> [a] -> Maybe ([a],[a])
splitInTwo size1 size2 list
  | size1 + size2 == length list
  , (pt1,pt2) <- splitAt size1 list
  = Just (pt1,pt2)
  | otherwise = Nothing

splitInThree :: Int -> Int -> Int -> [a] -> Maybe ([a],[a],[a])
splitInThree size1 size2 size3 list
  | size1 + size2 + size3 == length list
  , (pt1,rst) <- splitAt size1 list
  , (pt2,pt3) <- splitAt size2 rst
  = Just (pt1,pt2,pt3)
  | otherwise = Nothing

splitInMany :: [Int] -> [a] -> Maybe [[a]]
splitInMany []     []    = return []
splitInMany []     (_:_) = Nothing
splitInMany (s:ss) ls    = do
  let (pt,rest) = splitAt s ls
  guard (length pt == s)
  pts <- splitInMany ss rest
  return (pt:pts)


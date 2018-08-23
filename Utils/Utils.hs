{-# OPTIONS_GHC -Wall #-}

module Utils.Utils (chunksOf, toEnumError, fromEnumError, pair) where

import Data.List (unfoldr)
import qualified Data.ByteString as BS

chunksOf :: Int -> BS.ByteString -> [BS.ByteString]
chunksOf x = unfoldr gen
  where
    gen a | BS.null a = Nothing
          | otherwise = Just (BS.splitAt x a)

toEnumError :: (Show a) => String -> Int -> (a,a) -> b
toEnumError inst_ty i bnds
  = errorWithoutStackTrace
  $ "Enum.toEnum{" ++ inst_ty ++ "}: tag ("
    ++ show i ++ ") is outside of bounds "
    ++ show bnds

fromEnumError :: (Show a) => String -> a -> b
fromEnumError inst_ty x
  = errorWithoutStackTrace
  $ "Enum.fromEnum{" ++ inst_ty ++ "}: value ("
    ++ show x ++ ") is outside of Int's bounds "
    ++ show (minBound::Int, maxBound::Int)

pair :: [a] -> Maybe [(a,a)]
pair []       = Just []
pair (x:y:ys) = fmap ((x,y):) (pair ys)
pair (_:_)    = Nothing


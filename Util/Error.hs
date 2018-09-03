{-# OPTIONS_GHC -Wall #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Util.Error
-- Copyright   :  (c) Georgios Karachalias, 2018
-- License     :  BSD3
--
-- Maintainer  :  gdkaracha@gmail.com
-- Stability   :  experimental
-- Portability :  GHC
--
-- Error functions.
--
-----------------------------------------------------------------------------

module Util.Error
( toEnumError
, fromEnumError
, readHexError
) where

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

readHexError :: String -> String -> a
readHexError inst_ty str
  = errorWithoutStackTrace
  $ "Hex.readHex{" ++ inst_ty ++ "}: " ++ str



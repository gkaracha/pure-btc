{-# OPTIONS_GHC -Wall #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Encoding.WIF
-- Copyright   :  (c) Georgios Karachalias, 2018
-- License     :  BSD3
--
-- Maintainer  :  gdkaracha@gmail.com
-- Stability   :  experimental
-- Portability :  GHC
--
-- Wallet input format (WIF) encoding. Obsolete module.
--
-----------------------------------------------------------------------------

module Encoding.WIF
( -- * Wallet Input Format
  WIF(..)
) where

-- TODO: Make sure that a String rep is what you want

class WIF a where
  toWIF   :: a -> String
  fromWIF :: String -> a


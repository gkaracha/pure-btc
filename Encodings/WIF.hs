{-# OPTIONS_GHC -Wall #-}

module Encodings.WIF where


-- TODO: Make sure that a String rep is what you want

class WIF a where
  toWIF   :: a -> String
  fromWIF :: String -> a


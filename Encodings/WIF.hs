{-# OPTIONS_GHC -Wall #-}

module Encodings.WIF where


-- TODO: Of course it shouldn't be string but leave it like this for now, to be
--       able to parse it with our eyes.

class WIF a where
  toWIF   :: a -> String
  fromWIF :: String -> a


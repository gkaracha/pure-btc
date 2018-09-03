{-# OPTIONS_GHC -Wall #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Hash.HASH160
-- Copyright   :  (c) Georgios Karachalias, 2018
-- License     :  BSD3
--
-- Maintainer  :  gdkaracha@gmail.com
-- Stability   :  experimental
-- Portability :  GHC
--
-- Hash160.
--
-----------------------------------------------------------------------------

module Hash.HASH160
( -- * @'hash160'@
  hash160
) where

import Data.Words (ByteString)
import Hash.SHA256 (sha256)
import Hash.RIPEMD160 (ripemd160)

-- * 'hash160' function
-- ----------------------------------------------------------------------------

-- | Implementation of hashing function @'hash160'@.
--
-- > forall x. hash160 x == ripemd160 (sha256 x)
hash160 :: ByteString -> ByteString
hash160 = ripemd160 . sha256


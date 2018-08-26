{-# OPTIONS_GHC -Wall #-}

module Hash.HASH160 where

import Data.Words (ByteString)
import Hash.SHA256 (sha256)
import Hash.RIPEMD160 (ripemd160)

-- * 'hash160' function
-- ----------------------------------------------------------------------------

hash160 :: ByteString -> ByteString
hash160 = ripemd160 . sha256


{-# OPTIONS_GHC -Wall #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Key.Private
-- Copyright   :  (c) Georgios Karachalias, 2018
-- License     :  BSD3
--
-- Maintainer  :  gdkaracha@gmail.com
-- Stability   :  experimental
-- Portability :  GHC
--
-- Private keys.
--
-----------------------------------------------------------------------------

module Key.Private where -- (PrivateKey(..)) where
-- TODO: For now export everything

import qualified Data.ByteString as BS
import Control.Monad (guard)
import Control.Applicative ((<|>))

import Data.Words
import Encoding.Hex (Hex(..))
import Encoding.Base58Check (encodeBase58Check, decodeBase58Check)
import Util.Error (readHexError)

-- * `Uncompressed` private keys
-- ----------------------------------------------------------------------------

-- | Private key, to create uncompressed public keys
newtype UPrivateKey = UPriKey Word256
  deriving (Eq, Ord)

instance Hex UPrivateKey where
  showHex (UPriKey key) = showHex key
  readHex str
    | length str == 64 = UPriKey (readHex str)
    | otherwise        = readHexError "UPrivateKey" str

-- | `Uncompressed` private key to WIF format
upkToWIF :: UPrivateKey -> String
upkToWIF (UPriKey w) = encodeBase58Check 0x80 w

-- | Parse a 'ByteString' as an "uncompressed" private key
bsToUPK :: ByteString -> Maybe UPrivateKey
bsToUPK bs = do
  guard (BS.length bs == 32)
  UPriKey <$> fromByteString bs

-- * `Compressed` private keys
-- ----------------------------------------------------------------------------

-- | Private key, to create compressed public keys
newtype CPrivateKey = CPriKey Word256
  deriving (Eq, Ord)

instance Hex CPrivateKey where
  showHex (CPriKey key) = showHex key ++ "01"
  readHex str
    | (w,"01") <- splitAt 64 str = CPriKey (readHex w)
    | otherwise = readHexError "CPrivateKey" str

-- | `Compressed` private key to WIF format
cpkToWIF :: CPrivateKey -> String
cpkToWIF (CPriKey w) = encodeBase58Check 0x80
                         (fromBytes (toBytes w ++ [0x01]) :: ByteString)

-- | Parse a 'ByteString' as a "compressed" private key
bsToCPK :: ByteString -> Maybe CPrivateKey
bsToCPK bs = do
  guard (BS.length bs == 33)
  guard (BS.index bs 32 == 0x01)
  CPriKey <$> fromByteString (BS.take 32 bs)

-- * Private keys (`Compressed` / `Uncompressed`)
-- ----------------------------------------------------------------------------

data PrivateKey = UPrivateKey UPrivateKey | CPrivateKey CPrivateKey
  deriving (Eq)

instance Hex PrivateKey where
  showHex (UPrivateKey key) = showHex key
  showHex (CPrivateKey key) = showHex key
  readHex str
    | n == 66   = CPrivateKey (readHex str)
    | n == 64   = UPrivateKey (readHex str)
    | otherwise = readHexError "PrivateKey" str
    where n = length str

-- | Private key to WIF format
privateKeyToWIF :: PrivateKey -> String
privateKeyToWIF (UPrivateKey key) = upkToWIF key
privateKeyToWIF (CPrivateKey key) = cpkToWIF key

-- | WIF format to raw private key
wifToPrivateKey :: String -> Maybe PrivateKey
wifToPrivateKey str = do
  (vb,key,_checksum) <- decodeBase58Check str -- Decode the key (also checks checksum)
  guard (vb == 0x80)                          -- Make sure the version byte is 0x80
  (UPrivateKey <$> bsToUPK key <|>
   CPrivateKey <$> bsToCPK key)               -- Parse either (they are mutually exclusive)

-- test1 :: String
-- test1 = case wifToPrivateKey "5HueCGU8rMjxEXxiPuD5BDku4MkFqeZyd4dZ1jvhTVqvbTLvyTJ" of
--   Nothing -> "this-was-an-error"
--   Just k  -> showHex k
--              ++ case k of {UPrivateKey{} -> " (uncompressed)"; CPrivateKey{} -> " (compressed)" }
--
-- test1inv :: String
-- test1inv = privateKeyToWIF $ UPrivateKey $ UPK
--          $ 0x0C28FCA386C7A227600B2FE50B7CAE11EC86D3BF1FBE471BE89827E19D72AA1D



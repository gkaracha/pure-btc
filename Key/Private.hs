{-# OPTIONS_GHC -Wall #-}

module Key.Private where -- (PrivateKey(..)) where
-- TODO: For now export everything

import Data.Words
import Encodings.Hex

-- | Private key, to create uncompressed public keys
newtype UPrivateKey = UPK Word256

instance Hex UPrivateKey where
  showHex (UPK key) = showHex key
  readHex str = UPK (readHex str)

-- | Private key, to create compressed public keys
newtype CPrivateKey = CPK Word256

instance Hex CPrivateKey where
  showHex (CPK key) = showHex key ++ "01"
  readHex str
    | (w,"01") <- splitAt 128 str
    = CPK (readHex w)
    | otherwise = error $ "readHex{CPrivateKey}: " ++ str


-- |~| instance Hex PrivateKey where
-- |~|   showHex (PK key) = showHex key
-- |~|   readHex str = PK (readHex str)


-- | module Key.Public where -- Private (PrivateKey(..)) where
-- | -- TODO: For now export everything
-- |
-- | import Data.Words
-- | import Encodings.Hex
-- | import Utils.Utils (splitInThree)
-- | import Data.List (splitAt)
-- |
-- | -- | Uncompressed public key
-- | data UPublicKey = UPK Word256 Word256 -- (x,y), prefix 0x04)
-- |                                       -- shown as (8 + 256 + 256 = 520 bits)
-- |
-- | instance Hex UPublicKey where
-- |   showHex (UPK x y) = "04" ++ showHex x ++ showHex y
-- |   readHex str
-- |     | Just ("04",x,y) <- splitInThree 2 64 64 str
-- |     = UPK (readHex x) (readHex y)
-- |     | otherwise = error $ "readHex{UPublicKey}: " ++ str
-- |   -- characters: (2 + 64 + 64 = 130 hex digits)
-- |
-- | -- | Compressed public key
-- | data CPublicKey -- shown as (8 + 256 = 264 bits)
-- |   = CPKEven Word256 -- y is even (prefix 0x02)
-- |   | CPKOdd  Word256 -- y is odd  (prefix 0x03)
-- |
-- | instance Hex CPublicKey where
-- |   showHex w = case w of
-- |     CPKEven x -> "02" ++ showHex x
-- |     CPKOdd  x -> "03" ++ showHex x
-- |   readHex str
-- |     | length str == 66, (code,word) <- splitAt 2 str
-- |     , Just fn <-  pCPKcode code
-- |     = fn (readHex word)
-- |     | otherwise = error $ "readHex{CPublicKey}: " ++ str
-- |   -- characters: (2 + 64 = 66 hex digits)
-- |
-- | pCPKcode :: String -> Maybe (Word256 -> CPublicKey)
-- | pCPKcode "02" = Just CPKEven
-- | pCPKcode "03" = Just CPKOdd
-- | pCPKcode _    = Nothing



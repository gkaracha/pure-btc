{-# OPTIONS_GHC -Wall #-}

module Key.Address where
-- TODO: For now export everything

import Encodings.Hex
import Key.Public
import Encodings.Base58Check
import Hash.HASH160

-- * Addresses
-- ----------------------------------------------------------------------------

newtype Address = Addr String -- represent an address as string (base58check encoding)
  deriving (Eq, Ord)

-- | Create an address from a public key
createAddress :: PublicKey -> Address
createAddress = Addr
              . encodeBase58Check 0x00
              . hash160
              . pubKeyToByteString

instance Show Address where
  show (Addr addr) = addr

-- TODO: GIVE MORE OPERATIONS ON ADDRESSES: PARTITIONING, VALIDATION, CONVERSIONS
-- pubKeyToByteString :: PublicKey -> ByteString
-- encodeBase58Check :: Bytes a => Word8 {- version byte -} -> a {- payload -} -> String
-- hash160 :: ByteString -> ByteString

-- * Testing
-- ----------------------------------------------------------------------------

test :: Bool
test = createAddress (readHex "0202a406624211f2abbdc68da3df929f938c3399dd79fac1b51b0e4ad1d26a47aa")
       == Addr "1PRTTaJesdNovgne6Ehcdu1fpEdX7913CK"



{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ViewPatterns #-}

module Key.Public where -- Private (PrivateKey(..)) where
-- TODO: For now export everything

import Data.Words
import Encoding.Hex (Hex(..))
import Hash.ECM (hashECM, uncompress)
import Util.List (splitInTwo, splitInThree)
import Util.Error (readHexError)
import qualified Key.Private as PK

-- * Uncompressed public keys
-- ----------------------------------------------------------------------------

-- | Uncompressed public key
data UPublicKey = UPK Word256 Word256 -- (x,y), prefix 0x04, shown as (8 + 256 + 256 = 520 bits)
  deriving (Eq)

instance Hex UPublicKey where
  showHex (UPK x y) = "04" ++ showHex x ++ showHex y
  readHex str
    | Just ("04",x,y) <- splitInThree 2 64 64 str
    = UPK (readHex x) (readHex y)
    | otherwise = readHexError "UPublicKey" str
  -- characters: (2 + 64 + 64 = 130 hex digits)

-- * Compressed public keys
-- ----------------------------------------------------------------------------

-- | Compressed public key
data CPublicKey -- shown as (8 + 256 = 264 bits)
  = CPKEven Word256 -- y is even (prefix 0x02)
  | CPKOdd  Word256 -- y is odd  (prefix 0x03)
  deriving (Eq)

instance Hex CPublicKey where
  showHex w = case w of
    CPKEven x -> "02" ++ showHex x
    CPKOdd  x -> "03" ++ showHex x
  readHex str = case splitInTwo 2 64 str of
    Just (code,word)
      | code == "02" -> CPKEven $ readHex word
      | code == "03" -> CPKOdd  $ readHex word
    _other -> readHexError "CPublicKey" str
  -- characters: (2 + 64 = 66 hex digits)

-- * Public keys
-- ----------------------------------------------------------------------------

-- | Public key
data PublicKey = UPublicKey UPublicKey | CPublicKey CPublicKey
  deriving (Eq)

instance Hex PublicKey where
  showHex (UPublicKey key) = showHex key
  showHex (CPublicKey key) = showHex key
  readHex str
    | Just ("04",x,y) <- splitInThree 2 64 64 str
    = UPublicKey $ UPK (readHex x) (readHex y)
    | otherwise = case splitInTwo 2 64 str of
        Just (code,word)
          | code == "02" -> CPublicKey $ CPKEven $ readHex word
          | code == "03" -> CPublicKey $ CPKOdd  $ readHex word
        _other -> readHexError "PublicKey" str

-- * Turn a public key into a 'ByteString'
-- ----------------------------------------------------------------------------

pubKeyToByteString :: PublicKey -> ByteString
pubKeyToByteString (UPublicKey upub)
  = case upub of
      UPK w1 w2 -> fromBytes $ 0x04 : toBytes w1 ++ toBytes w2
pubKeyToByteString (CPublicKey cpub)
  = case cpub of
      CPKEven w -> fromBytes (0x02 : toBytes w)
      CPKOdd  w -> fromBytes (0x03 : toBytes w)

-- * Generate a public key from a private key
-- ----------------------------------------------------------------------------

publicKeyFromPrivateKey :: PK.PrivateKey -> PublicKey
publicKeyFromPrivateKey pk = case pk of
  PK.UPrivateKey key -> UPublicKey (publicKeyFromUPrivateKey key)
  PK.CPrivateKey key -> CPublicKey (publicKeyFromCPrivateKey key)
  where
    publicKeyFromUPrivateKey :: PK.UPrivateKey -> UPublicKey
    publicKeyFromUPrivateKey (PK.UPK (hashECM -> (x,y))) = UPK x y

    publicKeyFromCPrivateKey :: PK.CPrivateKey -> CPublicKey
    publicKeyFromCPrivateKey (PK.CPK (hashECM -> (x,y)))
      | even y    = CPKEven x
      | otherwise = CPKOdd  x

-- * Switch between compressed and uncompressed public keys
-- ----------------------------------------------------------------------------

compressPublicKey :: UPublicKey -> CPublicKey
compressPublicKey (UPK x y) | even y    = CPKEven x
                            | otherwise = CPKOdd  x

uncompressPublicKey :: CPublicKey -> UPublicKey
uncompressPublicKey (CPKEven x) = uncurry UPK $ uncompress 0x02 x
uncompressPublicKey (CPKOdd  x) = uncurry UPK $ uncompress 0x03 x

-- * Testing
-- ----------------------------------------------------------------------------

test1 :: Bool
test1 = showHex (publicKeyFromPrivateKey (readHex "3aba4162c7251c891207b747840551a71939b0de081f85c4e44cf7c13e41daa6"))
        == "045c0de3b9c8ab18dd04e3511243ec2952002dbfadc864b9628910169d9b9b00ec243bcefdd4347074d44bd7356d6a53c495737dd96295e2a9374bf5f02ebfc176"

test2 :: Bool
test2 = showHex (compressPublicKey (readHex "045c0de3b9c8ab18dd04e3511243ec2952002dbfadc864b9628910169d9b9b00ec243bcefdd4347074d44bd7356d6a53c495737dd96295e2a9374bf5f02ebfc176"))
        == "025c0de3b9c8ab18dd04e3511243ec2952002dbfadc864b9628910169d9b9b00ec"

test3 :: Bool
test3 = showHex (uncompressPublicKey (readHex "025c0de3b9c8ab18dd04e3511243ec2952002dbfadc864b9628910169d9b9b00ec"))
        == "045c0de3b9c8ab18dd04e3511243ec2952002dbfadc864b9628910169d9b9b00ec243bcefdd4347074d44bd7356d6a53c495737dd96295e2a9374bf5f02ebfc176"





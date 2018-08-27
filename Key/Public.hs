{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ViewPatterns #-}

module Key.Public where -- Private (PrivateKey(..)) where
-- TODO: For now export everything

import Data.Words
import Encodings.Hex
import Hash.ECM
import Utils.List (splitInTwo, splitInThree)
import Utils.Error (readHexError)
import qualified Key.Private as PK

-- * Uncompressed public keys
-- ----------------------------------------------------------------------------

-- | Uncompressed public key
data UPubKey = UPK Word256 Word256 -- (x,y), prefix 0x04, shown as (8 + 256 + 256 = 520 bits)
  deriving (Eq)

instance Hex UPubKey where
  showHex (UPK x y) = "04" ++ showHex x ++ showHex y
  readHex str
    | Just ("04",x,y) <- splitInThree 2 64 64 str
    = UPK (readHex x) (readHex y)
    | otherwise = readHexError "UPubKey" str
  -- characters: (2 + 64 + 64 = 130 hex digits)

-- * Compressed public keys
-- ----------------------------------------------------------------------------

-- | Compressed public key
data CPubKey -- shown as (8 + 256 = 264 bits)
  = CPKEven Word256 -- y is even (prefix 0x02)
  | CPKOdd  Word256 -- y is odd  (prefix 0x03)
  deriving (Eq)

instance Hex CPubKey where
  showHex w = case w of
    CPKEven x -> "02" ++ showHex x
    CPKOdd  x -> "03" ++ showHex x
  readHex str = case splitInTwo 2 64 str of
    Just (code,word)
      | code == "02" -> CPKEven $ readHex word
      | code == "03" -> CPKOdd  $ readHex word
    _other -> readHexError "CPubKey" str
  -- characters: (2 + 64 = 66 hex digits)

-- * Public keys
-- ----------------------------------------------------------------------------

-- | Public key
data PublicKey = UPubKey UPubKey | CPubKey CPubKey
  deriving (Eq)

instance Hex PublicKey where
  showHex (UPubKey key) = showHex key
  showHex (CPubKey key) = showHex key
  readHex str
    | Just ("04",x,y) <- splitInThree 2 64 64 str
    = UPubKey $ UPK (readHex x) (readHex y)
    | otherwise = case splitInTwo 2 64 str of
        Just (code,word)
          | code == "02" -> CPubKey $ CPKEven $ readHex word
          | code == "03" -> CPubKey $ CPKOdd  $ readHex word
        _other -> readHexError "PublicKey" str

-- * Turn a public key into a 'ByteString'
-- ----------------------------------------------------------------------------

pubKeyToByteString :: PublicKey -> ByteString
pubKeyToByteString (UPubKey upub)
  = case upub of
      UPK w1 w2 -> fromBytes $ 0x04 : toBytes w1 ++ toBytes w2
pubKeyToByteString (CPubKey cpub)
  = case cpub of
      CPKEven w -> fromBytes (0x02 : toBytes w)
      CPKOdd  w -> fromBytes (0x03 : toBytes w)

-- * Generate a public key from a private key
-- ----------------------------------------------------------------------------

publicKeyFromPrivateKey :: PK.PrivateKey -> PublicKey
publicKeyFromPrivateKey pk = case pk of
  PK.UPrivateKey key -> UPubKey (publicKeyFromUPrivateKey key)
  PK.CPrivateKey key -> CPubKey (publicKeyFromCPrivateKey key)
  where
    publicKeyFromUPrivateKey :: PK.UPrivateKey -> UPubKey
    publicKeyFromUPrivateKey (PK.UPK (hashECM -> (x,y))) = UPK x y

    publicKeyFromCPrivateKey :: PK.CPrivateKey -> CPubKey
    publicKeyFromCPrivateKey (PK.CPK (hashECM -> (x,y)))
      | even y    = CPKEven x
      | otherwise = CPKOdd  x

-- * Switch between compressed and uncompressed public keys
-- ----------------------------------------------------------------------------

compressPublicKey :: UPubKey -> CPubKey
compressPublicKey (UPK x y) | even y    = CPKEven x
                            | otherwise = CPKOdd  x

uncompressPublicKey :: CPubKey -> UPubKey
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





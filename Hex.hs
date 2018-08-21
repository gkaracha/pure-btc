{-# OPTIONS_GHC -Wall #-}

module Hex (Hex(..), showHexBytes, readHexDigit, readHexBytes) where
{-
  Convert from numeric types to hex strings and vice versa.
-}

import qualified Numeric         as N
import qualified Data.ByteString as BS
import Data.Char (ord)
import Data.List (foldl')
import Words
import Utils

-- * Class for things that can be printed to and read as hexadecimal
-- ----------------------------------------------------------------------------

class Hex a where
  showHex :: a -> String
  readHex :: String -> a

instance Hex Word8  where
  showHex = showHexWord8
  readHex = readHexBytes

instance Hex Word16 where
  showHex = showHexBytes
  readHex = readHexBytes

instance Hex Word32 where
  showHex = showHexBytes
  readHex = readHexBytes

instance Hex Word64 where
  showHex = showHexBytes
  readHex = readHexBytes

instance Hex Word128 where
  showHex = showHexBytes
  readHex = readHexBytes

instance Hex Word160 where
  showHex = showHexBytes
  readHex = readHexBytes

instance Hex Word256 where
  showHex = showHexBytes
  readHex = readHexBytes

instance Hex Word512 where
  showHex = showHexBytes
  readHex = readHexBytes

instance Hex BS.ByteString where
  showHex = showHexBytes
  readHex = readHexByteString

-- * Hexadecimal printing utilities
-- ----------------------------------------------------------------------------

-- | Show in hexadecimal anything that consists of bytes
showHexBytes :: Bytes a => a -> String
showHexBytes = concatMap showHexWord8 . toBytes

-- | Show a single byte in hexadecimal (2 hex digits)
showHexWord8 :: Word8 -> String
showHexWord8 w = replicate (2 - length h) '0' ++ h where h = N.showHex w ""

-- * Hexadecimal parsing utilities
-- ----------------------------------------------------------------------------

readHexBytes :: (Num a, Bytes a) => String -> a
readHexBytes str
  | noBytes result * 2 == length str = result -- each byte corresponds to two hex digits
  | otherwise = error "readHexBytes: length mismatch"
  where
    result = fromInteger
           $ foldl' (\acc x -> 16*acc + readHexDigit x) 0 str

readHexByteString :: String -> BS.ByteString
readHexByteString str
  | Just ps <- pair str = fromBytes (map fn ps)
  | otherwise = error "readHexByteString: length mismatch"
  where
    fn (c1,c2) = (readHexDigit c1 `rotateL` 4) .|. readHexDigit c2

readHexDigit :: Num a => Char -> a
readHexDigit c
  | ordc >= 48, ordc <= 57  {- 0..9 -} = fromIntegral (ordc - 48)
  | ordc >= 65, ordc <= 70  {- A..F -} = fromIntegral (ordc - 55)
  | ordc >= 97, ordc <= 102 {- a..f -} = fromIntegral (ordc - 87)
  | otherwise = error $ "readHexDigit: " ++ c : " is not in not a hex char"
  where
    ordc = ord c

    -- ord 'A' -> 65
    -- ord 'F' -> 70
    -- ord 'a' -> 97
    -- ord 'f' -> 102
    -- ord '0' -> 48
    -- ord '9' -> 57


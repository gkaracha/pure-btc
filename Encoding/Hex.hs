{-# OPTIONS_GHC -Wall #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Encoding.Hex
-- Copyright   :  (c) Georgios Karachalias, 2018
-- License     :  BSD3
--
-- Maintainer  :  gdkaracha@gmail.com
-- Stability   :  experimental
-- Portability :  GHC
--
-- Hexadeximal representation of unsigned integral types.
--
-----------------------------------------------------------------------------

module Encoding.Hex
( -- * The @'Hex'@ class
  Hex(..)

  -- * Utilities
, showHexBytes, readHexDigit, readHexBytes

  -- * IO operations
, getHexLine, getHexWords, getHexLines, printHex
) where
{-
  Convert from numeric types to hex strings and vice versa.
-}

import qualified Numeric         as N
import qualified Data.ByteString as BS
import Data.Char (ord)
import Data.List (foldl')
import Data.Words
import Util.List (pairs)

-- * Class for things that can be printed to and read as hexadecimal strings
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

-- | Show in hexadecimal anything that consists of bytes (can be used as the
-- default implementation of 'showHex' if the type is also an instance of
-- 'Bytes').
showHexBytes :: Bytes a => a -> String
showHexBytes = concatMap showHexWord8 . toBytes

-- | Show a single byte in hexadecimal (two hex digits).
showHexWord8 :: Word8 -> String
showHexWord8 w = replicate (2 - length h) '0' ++ h where h = N.showHex w ""

-- * Hexadecimal parsing utilities
-- ----------------------------------------------------------------------------

-- | Turn a hexadecimal string into a number.
readHexBytes :: (Num a, Bytes a) => String -> a
readHexBytes str
  | noBytes result * 2 == length str = result -- each byte corresponds to two hex digits
  | otherwise = error "readHexBytes: length mismatch"
  where
    result = fromInteger
           $ foldl' (\acc x -> 16*acc + readHexDigit x) 0 str

-- | Turn a hexadecimal string into a bytestring.
readHexByteString :: String -> BS.ByteString
readHexByteString str
  | Just ps <- pairs str = fromBytes (map fn ps)
  | otherwise = error "readHexByteString: length mismatch"
  where
    fn (c1,c2) = (readHexDigit c1 `rotateL` 4) .|. readHexDigit c2

-- | Turn a hexadecimal character into a number.
readHexDigit :: Num a => Char -> a
readHexDigit c
  | ordc >= 48, ordc <= 57  {- 0..9 -} = fromIntegral (ordc - 48)
  | ordc >= 65, ordc <= 70  {- A..F -} = fromIntegral (ordc - 55)
  | ordc >= 97, ordc <= 102 {- a..f -} = fromIntegral (ordc - 87)
  | otherwise = error $ "readHexDigit: " ++ c : " is not in not a hex char"
  where
    ordc = ord c
    -- ord 'A' -> 65, 'F' -> 70, 'a' -> 97, 'f' -> 102, '0' -> 48, '9' -> 57

-- * Hexadecimal strings input / output
-- ----------------------------------------------------------------------------

-- | Parse a line as a hexadecimal.
getHexLine :: Hex a => IO a
getHexLine = readHex <$> getLine

-- | Parse a line as a space-separated hexadecimals.
getHexWords :: Hex a => IO [a]
getHexWords = map readHex . words <$> getLine

-- | Parse a file as a newline-separated hexadecimals.
getHexLines :: Hex a => IO [a]
getHexLines = map readHex . lines <$> getContents

-- | Print something in hexadecimal and change line.
printHex :: Hex a => a -> IO ()
printHex = putStrLn . showHex


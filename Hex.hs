{-# OPTIONS_GHC -Wall #-}

module Hex where
{-
  Convert from numeric types to hex strings and vice versa.
-}

import qualified Numeric         as N
import qualified Data.ByteString as BS
import Data.Char (ord)
import Data.List (foldl')
import Types

class Hex a where
  showHex :: a -> String
  readHex :: String -> a

instance Hex Word8  where
  showHex = showFiniteHex
  readHex = readHexI

instance Hex Word16 where
  showHex = showFiniteHex
  readHex = readHexI

instance Hex Word32 where
  showHex = showFiniteHex
  readHex = readHexI

instance Hex Word64 where
  showHex = showFiniteHex
  readHex = readHexI

instance Hex Word256 where
  showHex (W256 w1 w2 w3 w4 w5 w6 w7 w8) = concatMap showHex [w1,w2,w3,w4,w5,w6,w7,w8]

instance Hex Word512 where
  showHex (W512 w1 w2 w3 w4 w5 w6 w7 w8 w9 w10 w11 w12 w13 w14 w15 w16)
    = concatMap showHex [w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15,w16]

instance Hex BS.ByteString where
  showHex = concatMap showHex . BS.unpack

class {- FiniteBits a => -} HexSize a where
  hSize :: a -> Int

instance HexSize Word8   where { hSize _ = 2   }
instance HexSize Word16  where { hSize _ = 4   }
instance HexSize Word32  where { hSize _ = 8   }
instance HexSize Word64  where { hSize _ = 16  }
-- Word128 ?
instance HexSize Word256 where { hSize _ = 64  }
instance HexSize Word512 where { hSize _ = 128 }

instance HexSize BS.ByteString where
  hSize bs = 2 * BS.length bs

showFiniteHex :: (Integral a, Show a, HexSize a) => a -> String
showFiniteHex w = replicate (hSize w - length h) '0' ++ h where h = N.showHex w ""

readHexI :: Integral a => String -> a
readHexI = fromIntegral . foldl' (\acc x -> 16*acc + readHexDigit x) 0

ensureLength :: String -> Int -> ()
ensureLength s i = if length s == i then () else error "ensureLength failed"

readHexDigit :: Char -> Integer
readHexDigit c
  | ordc >= 48, ordc <= 57  {- 0..9 -} = fromIntegral (ordc - 48)
  | ordc >= 65, ordc <= 70  {- A..F -} = fromIntegral (ordc - 55)
  | ordc >= 97, ordc <= 102 {- a..f -} = fromIntegral (ordc - 87)
  | otherwise = error "readHexDigit failed"
  where
    ordc = ord c

    -- ord 'A' -> 65
    -- ord 'F' -> 70
    -- ord 'a' -> 97
    -- ord 'f' -> 102
    -- ord '0' -> 48
    -- ord '9' -> 57




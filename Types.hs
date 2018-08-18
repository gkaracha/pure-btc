{-# OPTIONS_GHC -Wall #-}

module Types
( -- Re-exported from Data.Word
  Word8, Word16, Word32, Word64

  -- Conversion Functions
, splitWord64, concatWord32s
, splitWord32, concatWord16s
, splitWord16, concatWord8s
, w64_w16, w16_w64
, w64_w8,  w8_w64
, w32_w8,  w8_w32

  -- Words of size 256 and 512 bits
, Word256(..), Word512(..)

  -- Split a ByteString in chunks
, chunksOf

  -- Read words from ByteStrings
, bs_w8, bs_w16, bs_w32, bs_w64

  -- Other utilities
, w64_bytes
) where

{-
  The types we will use in all other modules
-}

import qualified Data.ByteString as BS
import Data.Word
import Data.Bits
import Data.List (unfoldr)

-- * Partitioning and concatenation of different word sizes
-- ----------------------------------------------------------------------------

-- | Split a 'Word64' into two 'Word32's.
splitWord64 :: Word64 -> (Word32,Word32)
splitWord64 w = (pt1,pt2)
  where
    pt1 = fromIntegral ((w .&. 0xFFFFFFFF00000000) `rotateR` 32)
    pt2 = fromIntegral (w .&. 0x00000000FFFFFFFF)

-- | Concatenate two 'Word32's into a 'Word64'.
concatWord32s :: (Word32, Word32) -> Word64
concatWord32s (w1,w2) = pt1 .|. pt2
  where
    pt1 = fromIntegral w1 `rotateL` 32
    pt2 = fromIntegral w2

-- | Split a 'Word32' into two 'Word16's.
splitWord32 :: Word32 -> (Word16,Word16)
splitWord32 w = (pt1,pt2)
  where
    pt1 = fromIntegral ((w .&. 0xFFFF0000) `rotateR` 16)
    pt2 = fromIntegral (w .&. 0x0000FFFF)

-- | Concatenate two 'Word16's into a 'Word32'.
concatWord16s :: (Word16,Word16) -> Word32
concatWord16s (w1,w2) = pt1 .|. pt2
  where
    pt1 = fromIntegral w1 `rotateL` 16
    pt2 = fromIntegral w2

-- | Split a 'Word16' into two 'Word8's.
splitWord16 :: Word16 -> (Word8,Word8)
splitWord16 w = (pt1,pt2)
  where
    pt1 = fromIntegral ((w .&. 0xFF00) `rotateR` 8)
    pt2 = fromIntegral (w .&. 0x00FF)

-- | Concatenate two 'Word8's into a 'Word16'.
concatWord8s :: (Word8,Word8) -> Word16
concatWord8s (w1,w2) = pt1 .|. pt2
  where
    pt1 = fromIntegral w1 `rotateL` 8
    pt2 = fromIntegral w2

-- ----------------------------------------------------------------------------

-- | Split a 'Word64' into four 'Word16's.
w64_w16 :: Word64 -> (Word16,Word16,Word16,Word16)
w64_w16 w = (w1,w2,w3,w4)
  where
    (w12,w34) = splitWord64 w
    (w1,w2) = splitWord32 w12
    (w3,w4) = splitWord32 w34

-- | Concatenate four 'Word16's into a 'Word64'.
w16_w64 :: (Word16,Word16,Word16,Word16) -> Word64
w16_w64 (w1,w2,w3,w4) = concatWord32s (concatWord16s (w1,w2),concatWord16s (w3,w4))

-- | Split a 'Word64' into eight 'Word8's.
w64_w8 :: Word64 -> (Word8,Word8,Word8,Word8,Word8,Word8,Word8,Word8)
w64_w8 w = (w1,w2,w3,w4,w5,w6,w7,w8)
  where
    (w12,w34,w56,w78) = w64_w16 w
    (w1,w2) = splitWord16 w12
    (w3,w4) = splitWord16 w34
    (w5,w6) = splitWord16 w56
    (w7,w8) = splitWord16 w78

-- | Concatenate eight 'Word8's into a 'Word64'.
w8_w64 :: (Word8,Word8,Word8,Word8,Word8,Word8,Word8,Word8) -> Word64
w8_w64 (w1,w2,w3,w4,w5,w6,w7,w8)
  = concatWord32s (w8_w32 (w1,w2,w3,w4), w8_w32 (w5,w6,w7,w8))

-- | Split a 'Word32' into four 'Word8's.
w32_w8 :: Word32 -> (Word8,Word8,Word8,Word8)
w32_w8 w = (w1,w2,w3,w4)
  where
    (w12,w34) = splitWord32 w
    (w1,w2) = splitWord16 w12
    (w3,w4) = splitWord16 w34

-- | Concatenate four 'Word8's into a 'Word32'.
w8_w32 :: (Word8,Word8,Word8,Word8) -> Word32
w8_w32 (w1,w2,w3,w4) = concatWord16s (concatWord8s (w1,w2),concatWord8s (w3,w4))

-- * Bigger words to use when dealing with BTC-related operations
-- ----------------------------------------------------------------------------

data Word256
  = W256 { w256_0 :: Word32
         , w256_1 :: Word32
         , w256_2 :: Word32
         , w256_3 :: Word32
         , w256_4 :: Word32
         , w256_5 :: Word32
         , w256_6 :: Word32
         , w256_7 :: Word32 }

data Word512
  = W512 { w512_00 :: Word32
         , w512_01 :: Word32
         , w512_02 :: Word32
         , w512_03 :: Word32
         , w512_04 :: Word32
         , w512_05 :: Word32
         , w512_06 :: Word32
         , w512_07 :: Word32
         , w512_08 :: Word32
         , w512_09 :: Word32
         , w512_10 :: Word32
         , w512_11 :: Word32
         , w512_12 :: Word32
         , w512_13 :: Word32
         , w512_14 :: Word32
         , w512_15 :: Word32 }

-- * Parsing 'ByteString's as Words
-- ----------------------------------------------------------------------------

chunksOf :: Int -> BS.ByteString -> [BS.ByteString]
chunksOf x = unfoldr gen
  where
    gen a | BS.null a = Nothing
          | otherwise = Just (BS.splitAt x a)

bs_w8 :: BS.ByteString -> Word8
bs_w8 bs = case BS.unpack bs of
  [w1]   -> w1
  _other -> error "bs_w8"

bs_w16 :: BS.ByteString -> Word16
bs_w16 bs = case BS.unpack bs of
  [w1,w2] -> concatWord8s (w1,w2)
  _other  -> error "bs_w16"

bs_w32 :: BS.ByteString -> Word32
bs_w32 bs = case BS.unpack bs of
  [w1,w2,w3,w4] -> w8_w32 (w1,w2,w3,w4)
  _other        -> error "bs_w32"

bs_w64 :: BS.ByteString -> Word64
bs_w64 bs = case BS.unpack bs of
  [w1,w2,w3,w4,w5,w6,w7,w8] -> w8_w64 (w1,w2,w3,w4,w5,w6,w7,w8)
  _other                    -> error "bs_w64"

w64_bytes :: Word64 -> [Word8]
w64_bytes w | (w1,w2,w3,w4,w5,w6,w7,w8) <- w64_w8 w = [w1,w2,w3,w4,w5,w6,w7,w8]

-- TODO: We need to give instances for a gazillion
--       classes. I guess we'll do it eventually.


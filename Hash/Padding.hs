{-# OPTIONS_GHC -Wall #-}

module Hash.Padding (padAndChunkBS, padRIPEMD160, bytesToRIPEMD160, ripemd160ToBytes) where

-- padAndChunkBS :: BS.ByteString -> [Word512]

import qualified Data.ByteString as BS
import Utils.Utils
import Data.Words

-- * Message padding
-- ----------------------------------------------------------------------------

-- | Pad a message and partition it into chunks of 512 bits
padAndChunkBS :: BS.ByteString -> [Word512]
padAndChunkBS bs = map fromByteStringUnsafe
                 $ chunksOf 64
                 $ BS.concat [ bs                                           -- message
                             , BS.pack (0x80 : replicate (no_bytes-1) 0x00) -- 1 0..0
                             , toByteString (fromIntegral len :: Word64) ]  -- 64
  where
    -- Length of message
    len :: Int
    len = 8 * BS.length bs

    -- Number of bytes to add (1 0..0)
    no_bytes | (q,r) <- (comp_k len+1) `quotRem` 8
             = if r /= 0 then error "padBS: what??" else q

    comp_k :: Int -> Int
    comp_k l = case mod (l + 1 + 64) 512 of { 0 -> 0; n -> 512 - n }

-- * Message padding
-- ----------------------------------------------------------------------------

-- | Pad a message and partition it into chunks of 512 bits
padRIPEMD160 :: BS.ByteString -> [Word512]
padRIPEMD160 bs = bytesToRIPEMD160 all_bytes
  where
    all_bytes :: [Word8]
    all_bytes = concat [ toBytes bs
                       , 0x80 : replicate (no_bytes-1) 0x00
                       , reverse $ toBytes (fromIntegral len :: Word64) ]

    -- Length of message
    len :: Int
    len = 8 * BS.length bs

    -- Number of bytes to add (1 0..0)
    no_bytes | (q,r) <- (comp_k len+1) `quotRem` 8
             = if r /= 0 then error "padBS: what??" else q

    comp_k :: Int -> Int
    comp_k l = case mod (l + 1 + 64) 512 of { 0 -> 0; n -> 512 - n }


bytesToRIPEMD160 :: [Word8] -> [Word512]
bytesToRIPEMD160 bytes = word512s
  where
    word512s :: [Word512]
    word512s = map fromWords chunk512s

    chunk512s :: [[Word32]]
    chunk512s = listChunksOf 16 word32s

    word32s :: [Word32]
    word32s = map (fromBytes . reverse) chunk32s

    chunk32s :: [[Word8]]
    chunk32s = listChunksOf 4 bytes

-- inverse of bytesToRIPEMD160
ripemd160ToBytes :: Word160 -> [Word8]
ripemd160ToBytes = concatMap toBytes . map revWord32 . toWords

-- | error "not yet"
-- |   where
-- |
-- |
-- |
-- |     w1,w2,w3,w4,w5 :: Word32
-- |     [w1,w2,w3,w4,w5] = toWords w160


revWord32 :: Word32 -> Word32
revWord32 = fromBytes . reverse . toBytes













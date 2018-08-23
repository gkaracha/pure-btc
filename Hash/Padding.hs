{-# OPTIONS_GHC -Wall #-}

module Hash.Padding (padAndChunkBS) where

-- padAndChunkBS :: BS.ByteString -> [Word512]

import qualified Data.ByteString as BS
import Utils.Utils
import Data.Words

-- * Message padding
-- ----------------------------------------------------------------------------

-- | Padd a message and partition it into chunks of 512 bits
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


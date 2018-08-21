{-# OPTIONS_GHC -Wall #-}

module SHA256 where

import Data.Array (Array, listArray, (!), array)
import qualified Data.ByteString as BS

import Hex
import Utils
import Words

-- * Program Constants
-- ----------------------------------------------------------------------------

h01,h02,h03,h04,h05,h06,h07,h08 :: Word32
h01 = 0x6a09e667
h02 = 0xbb67ae85
h03 = 0x3c6ef372
h04 = 0xa54ff53a
h05 = 0x510e527f
h06 = 0x9b05688c
h07 = 0x1f83d9ab
h08 = 0x5be0cd19

-- | Table of constants, being the first 32 bits of the fractional parts of the
-- cube roots of the first 64 primes
k64 :: Array Int Word32
k64 = listArray (0,63)
        [ 0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5
        , 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5
        , 0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3
        , 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174
        , 0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc
        , 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da
        , 0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7
        , 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967
        , 0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13
        , 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85
        , 0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3
        , 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070
        , 0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5
        , 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3
        , 0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208
        , 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2 ]

-- * The Logical Auxiliary Functions
-- ----------------------------------------------------------------------------

ch :: Word32 -> Word32 -> Word32 -> Word32
ch x y z = (x .&. y) `xor` (complement x .&. z)

maj :: Word32 -> Word32 -> Word32 -> Word32
maj x y z = (x .&. y) `xor` (x .&. z) `xor` (y .&. z)

uS0 :: Word32 -> Word32
uS0 x = (x `rotateR` 2) `xor` (x `rotateR` 13) `xor` (x `rotateR` 22)

uS1 :: Word32 -> Word32
uS1 x = (x `rotateR` 6) `xor` (x `rotateR` 11) `xor` (x `rotateR` 25)

lS0 :: Word32 -> Word32
lS0 x = (x `rotateR` 7) `xor` (x `rotateR` 18) `xor` (x `shiftR` 3)

lS1 :: Word32 -> Word32
lS1 x = (x `rotateR` 17) `xor` (x `rotateR` 19) `xor` (x `shiftR` 10)

-- * Testing
-- ----------------------------------------------------------------------------

sha256_io_bytestring :: BS.ByteString -> IO ()
sha256_io_bytestring = putStrLn . showHex . sha256 . padAndChunkBS

-- |?| -- if you want to try a string, not a file, do
-- |?| --
-- |?| -- $ echo -n abc | sha256sum
-- |?| -- ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad  -
-- |?| -- $ echo -n "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq" | sha256sum
-- |?| -- 248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1  -
-- |?|
-- |?| -- "abc"
-- |?| --   ==> ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad
-- |?|
-- |?| -- "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
-- |?| --   ==> 248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1
-- |?|

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
    no_bytes | (d,r) <- (comp_k len+1) `quotRem` 8
             = if r /= 0 then error "padBS: what??" else d

comp_k :: Int -> Int
comp_k l = case mod (l + 1 + 64) 512 of
             0 -> 0
             n -> 512 - n

-- TODO: Create an alternative interface that allows for double hashing etc.

-- * Auxiliary types and operations
-- ----------------------------------------------------------------------------

-- | A 'Word256' partitioned in eight 'Word32's
type PartWord256 = (Word32,Word32,Word32,Word32,Word32,Word32,Word32,Word32)

partWord256ToWord256 :: PartWord256 -> Word256
partWord256ToWord256 (w1,w2,w3,w4,w5,w6,w7,w8) = fromWords [w1,w2,w3,w4,w5,w6,w7,w8]

addPartWord256 :: PartWord256 -> PartWord256 -> PartWord256
addPartWord256 (wa1,wa2,wa3,wa4,wa5,wa6,wa7,wa8) (wb1,wb2,wb3,wb4,wb5,wb6,wb7,wb8)
  = (wa1+wb1,wa2+wb2,wa3+wb3,wa4+wb4,wa5+wb5,wa6+wb6,wa7+wb7,wa8+wb8)

buildW :: Word512 -> Array Int Word32
buildW block = w
  where
    w = array (0,63) ((zip [0..15] (toWords block))
                      ++ [ (j, lS1 (w!(j-2)) + (w!(j-7)) + lS0 (w!(j-15)) + (w!(j-16)))
                         | j <- [16..63] ])

-- * SHA256 Hashing
-- ----------------------------------------------------------------------------

sha256 :: [Word512] -> Word256
sha256 mn = partWord256ToWord256 $ loop mn (h01,h02,h03,h04,h05,h06,h07,h08)
  where
    -- | SHA256 Compression Function
    comp_fn :: Word512 -> PartWord256 -> PartWord256
    comp_fn block hs = aux 0 hs
      where
        w :: Array Int Word32
        w = buildW block

        aux :: Int -> PartWord256 -> PartWord256
        aux j (a,b,c,d,e,f,g,h)
          | j > 63    = (a,b,c,d,e,f,g,h)
          | otherwise = let t1 = h + (uS1 e) + (ch e f g) + (k64 ! j) + (w ! j) in
                        let t2 = (uS0 a) + (maj a b c) in
                        aux (j+1) (t1+t2,a,b,c,d+t1,e,f,g)

    -- | Main Loop
    loop :: [Word512] -> PartWord256 -> PartWord256
    loop []             hpart = hpart
    loop (block:blocks) hpart = loop blocks (hpart' `addPartWord256` hpart)
      where hpart' = comp_fn block hpart


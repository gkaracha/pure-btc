{-# OPTIONS_GHC -Wall #-}

module Hash.RIPEMD160 (ripemd160, ripemd160N, doubleRIPEMD160, test_io_ripemd160) where

-- SOURCE:
--   https://www.esat.kuleuven.be/cosic/publications/article-56.pdf

import Data.Array (Array, listArray, (!))
import Encodings.Hex
import Data.Words
import Hash.Padding (padAndChunkBS)

-- * Program Constants
-- ----------------------------------------------------------------------------

-- | Initial values
iv0,iv1,iv2,iv3,iv4 :: Word32
iv0 = 0x67452301
iv1 = 0xEFCDAB89
iv2 = 0x98BADCFE
iv3 = 0x10325476
iv4 = 0xC3D2E1F0

-- | Added hexadecimal constants (Part A)
k80 :: Array Int Word32
k80 = listArray (0,79) . concat . map (replicate 16)
    $ [0x00000000,0x5A827999,0x6ED9EBA1,0x8F1BBCDC,0xA953FD4E]

-- | Added hexadecimal constants (Part B)
k'80 :: Array Int Word32
k'80 = listArray (0,79) . concat . map (replicate 16)
     $ [0x50A28BE6,0x5C4DD124,0x6D703EF3,0x7A6D76E9,0x00000000]

-- * Message word selection
-- ----------------------------------------------------------------------------

-- | Selection of message word (Part A)
r80 :: Array Int Int
r80 = listArray (0,79)
        [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15
        , 7, 4, 13, 1, 10, 6, 15, 3, 12, 0, 9, 5, 2, 14, 11, 8
        , 3, 10, 14, 4, 9, 15, 8, 1, 2, 7, 0, 6, 13, 11, 5, 12
        , 1, 9, 11, 10, 0, 8, 12, 4, 13, 3, 7, 15, 14, 5, 6, 2
        , 4, 0, 5, 9, 7, 12, 2, 10, 14, 1, 3, 8, 11, 6, 15, 13 ]

-- | Selection of message word (Part B)
r'80 :: Array Int Int
r'80 = listArray (0,79)
         [ 5, 14, 7, 0, 9, 2, 11, 4, 13, 6, 15, 8, 1, 10, 3, 12
         , 6, 11, 3, 7, 0, 13, 5, 10, 14, 15, 8, 12, 4, 9, 1, 2
         , 15, 5, 1, 3, 7, 14, 6, 9, 11, 8, 12, 2, 10, 0, 4, 13
         , 8, 6, 4, 1, 3, 11, 15, 0, 5, 12, 2, 13, 9, 7, 10, 14
         , 12, 15, 10, 4, 1, 5, 8, 7, 6, 2, 13, 14, 0, 3, 9, 11 ]

-- * Left rotations
-- ----------------------------------------------------------------------------

-- | Selection of left rotation (Part A)
s80 :: Array Int Int
s80 = listArray (0,79)
        [ 11, 14, 15, 12, 5, 8, 7, 9, 11, 13, 14, 15, 6, 7, 9, 8
        , 7, 6, 8, 13, 11, 9, 7, 15, 7, 12, 15, 9, 11, 7, 13, 12
        , 11, 13, 6, 7, 14, 9, 13, 15, 14, 8, 13, 6, 5, 12, 7, 5
        , 11, 12, 14, 15, 14, 15, 9, 8, 9, 14, 5, 6, 8, 6, 5, 12
        , 9, 15, 5, 11, 6, 8, 13, 12, 5, 12, 13, 14, 11, 8, 5, 6 ]

-- | Selection of left rotation (Part B)
s'80 :: Array Int Int
s'80 = listArray (0,79)
        [ 8, 9, 9, 11, 13, 15, 15, 5, 7, 7, 8, 11, 14, 14, 12, 6
        , 9, 13, 15, 7, 12, 8, 9, 11, 7, 7, 12, 7, 6, 15, 13, 11
        , 9, 7, 15, 11, 8, 6, 6, 14, 12, 13, 5, 14, 13, 13, 7, 5
        , 15, 5, 8, 11, 14, 14, 6, 14, 6, 9, 12, 9, 12, 5, 15, 8
        , 8, 5, 12, 9, 12, 5, 14, 6, 8, 13, 6, 5, 15, 13, 11, 11 ]

-- * The Logical Auxiliary Functions
-- ----------------------------------------------------------------------------

-- || f1 :: Word32 -> Word32 -> Word32 -> Word32
-- || f1 x y z = x `xor` y `xor` z                   -- i in [0..15]
-- ||
-- || f2 :: Word32 -> Word32 -> Word32 -> Word32
-- || f2 x y z = (x .&. y) .|. (complement x .&. z)  -- i in [16..31]
-- ||
-- || f3 :: Word32 -> Word32 -> Word32 -> Word32
-- || f3 x y z = (x .|. complement y) `xor` z        -- i in [32..47]
-- ||
-- || f4 :: Word32 -> Word32 -> Word32 -> Word32
-- || f4 x y z = (x .&. z) .|. (y .&. complement z)  -- i in [48..63]
-- ||
-- || f5 :: Word32 -> Word32 -> Word32 -> Word32
-- || f5 x y z = x `xor` (y .|. complement z)        -- i in [64..79]

-- All-in-one version, like in the paper
f :: Int -> Word32 -> Word32 -> Word32 -> Word32
f j x y z | j >=  0 && j <= 15 = x `xor` y `xor` z
          | j >= 16 && j <= 31 = (x .&. y) .|. (complement x .&. z)
          | j >= 32 && j <= 47 = (x .|. complement y) `xor` z
          | j >= 48 && j <= 63 = (x .&. z) .|. (y .&. complement z)
          | j >= 64 && j <= 79 = x `xor` (y .|. complement z)
          | otherwise = error $ "f: j = " ++ show j ++ " is out of bounds (0,79)"

-- * Auxiliary types and operations
-- ----------------------------------------------------------------------------

-- | A 'Word256' partitioned in eight 'Word32's
type PartWord160 = (Word32,Word32,Word32,Word32,Word32)

partWord160ToWord160 :: PartWord160 -> Word160
partWord160ToWord160 (w1,w2,w3,w4,w5) = fromWords [w1,w2,w3,w4,w5]

buildX :: Word512 -> Array Int Word32
buildX = listArray (0,15) . toWords

-- * RIPEMD160 Hashing (follow the paper to the letter; not the most efficient)
-- ----------------------------------------------------------------------------

-- GEORGE: THE ALGORITHM IMPLEMENTATION DEALS WITH STUFF USING BIG-BIT, BIG-BYTE ENDIAN

ripemd160hash :: [Word512] -> Word160
ripemd160hash input = partWord160ToWord160 $ loop input (iv0,iv1,iv2,iv3,iv4)
  where
    -- | RIPEMD160 Compression Function
    comp_fn :: Word512
            -> (PartWord160, PartWord160) -- ((a,b,c,d,e),(a',b',c',d',e')) input
            -> (PartWord160, PartWord160) -- ((a,b,c,d,e),(a',b',c',d',e')) output
    comp_fn block vars = aux 0 vars
      where
        x :: Array Int Word32
        x = buildX block

        aux :: Int -> (PartWord160, PartWord160) -> (PartWord160, PartWord160)
        aux j ((a,b,c,d,e),(a',b',c',d',e'))
          | j > 79    = ((a,b,c,d,e),(a',b',c',d',e'))
          | otherwise
          = let t  = ((a + f j      b  c  d + (x ! (r80  ! j)) + (k80  ! j)) `rotateL` (s80  ! j)) + e  in
            let t' = ((a'+ f (79-j) b' c' d'+ (x ! (r'80 ! j)) + (k'80 ! j)) `rotateL` (s'80 ! j)) + e' in
            let abcds  = (e, t, b, c  `rotateL` 10,d ) in
            let abcds' = (e',t',b',c' `rotateL` 10,d') in
            aux (j+1) (abcds,abcds')

    -- | Compute the next h0..h4
    comp_hs :: PartWord160 -> (PartWord160,PartWord160) -> PartWord160
    comp_hs (h0,h1,h2,h3,h4) ((a,b,c,d,e),(a',b',c',d',e'))
      = (h1+c+d', h2+d+e', h3+e+a', h4+a+b', h0+b+c')

    -- | Main Loop
    loop :: [Word512] -> PartWord160 -> PartWord160
    loop []             hpart = hpart
    loop (block:blocks) hpart = loop blocks (comp_hs hpart abcds)
      where abcds = comp_fn block (hpart,hpart)

-- TESTS
-- -----
-- | "a"   => "0bdc9d2d256b3ee9daae347be6f4dc835a467ffe"
-- | "abc" => "8eb208f7e05d987a9b044a8e98c6b087f15a0bfc"

-- start the h0..h4 with iv0..iv4

-- for i = [0..t-1] {
--   let a  = h0 in
--   let b  = h1 in
--   let c  = h2 in
--   let d  = h3 in
--   let e  = h4 in
--
--   let a' = h0 in
--   let b' = h1 in
--   let c' = h2 in
--   let d' = h3 in
--   let e' = h4 in
--
--   for j = [0..79] {
--     let t  = ..?1?..      in
--     let a  = e            in
--     let e  = d            in
--     let d  = rol_10 (c)   in
--     let c  = b            in
--     let b  = t            in
--
--     let t' = ..?2?..      in
--     let a' = e'           in
--     let e' = d'           in
--     let d' = rol_10 (c')  in
--     let c' = b'           in
--     let b' = t'           in
--   }
--   let t  = h1 + c + d' in
--   let h1 = h2 + d + e' in
--   let h2 = h3 + e + a' in
--   let h3 = h4 + a + b' in
--   let h4 = h0 + b + c' in
--   let h0 = t           in
-- }

-- * Testing
-- ----------------------------------------------------------------------------

test_io_ripemd160 :: ByteString -> IO ()
test_io_ripemd160 bs = putStrLn $ showHex $ ripemd160 bs

-- CHECKOUT THIS GUY (ENDIANESS):
--   http://www.users.zetnet.co.uk/hopwood/crypto/scan/md.html
-- AND THIS GUY:
--   https://github.com/sipa/Coin25519/blob/master/src/crypto/ripemd160.c
-- AND DEFINITELY THIS GUY:
--   https://crypto.stackexchange.com/questions/32400/how-does-ripemd160-pad-the-message

-- * RIPEMD160 Interface
-- ----------------------------------------------------------------------------

-- | Apply the RIPEMD160 algorithm on a bytestring
ripemd160 :: ByteString -> ByteString
ripemd160 = toByteString . ripemd160hash . padAndChunkBS

-- | -- | Apply the RIPEMD160 algorithm on a bytestring
-- | ripemd160 :: ByteString -> ByteString
-- | ripemd160 = toByteString . ripemd160hash
-- |           . (reverse . map (fromBytes . reverse . toBytes))
-- |           . padAndChunkBS

-- | Apply the RIPEMD160 algorithm N times on a bytestring
ripemd160N :: Int -> ByteString -> ByteString
ripemd160N n msg | n <= 0    = msg
                 | otherwise = ripemd160N (n-1) (ripemd160 msg)

-- | Apply the RIPEMD160 algorithm two times on a bytestring
doubleRIPEMD160 :: ByteString -> ByteString
doubleRIPEMD160 = ripemd160 . ripemd160


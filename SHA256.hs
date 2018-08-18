{-# OPTIONS_GHC -Wall #-}

module SHA256 where

-- Keep in mind:
-- * https://stackoverflow.com/questions/1959715/how-to-print-integer-literals-in-binary-or-hex-in-haskell
-- * https://stackoverflow.com/questions/47446588/difference-between-data-bytestring-and-data-bytestring-char8
-- * http://www.iwar.org.uk/comsec/resources/cipher/sha256-384-512.pdf

import Data.Word (Word32)
import Numeric (showHex, showIntAtBase)
import Data.Char (ord, intToDigit)
import Data.Bits
import Data.List.Split (chunksOf)
import Data.Array (Array, listArray, (!), array)

import qualified Types as T
import qualified Data.ByteString as BS

-- Conversions and Padding Utilities
-- ============================================================================

nZeros :: Int -> String
nZeros n = replicate n '0'

-- | Add zeros so that it has length n
zeroFillUpTo :: Int -> String -> String
zeroFillUpTo n s = nZeros (n - length s) ++ s

-- | Convert a character to its byte representation (8 bits)
charToBinaryString :: Char -> String
charToBinaryString c = zeroFillUpTo 8 $ showIntAtBase 2 intToDigit (ord c) ""

-- | Convert an integer to a 64-bit binary (64 bits)
intToBinaryString :: Int -> String
intToBinaryString i = zeroFillUpTo 64 $ showIntAtBase 2 intToDigit i ""

-- | Convert a character to a collection of bytes (binary)
stringToBinaryString :: String -> String -- Split in bytes
stringToBinaryString = concatMap charToBinaryString

comp_k :: Int -> Int
comp_k l = case mod (l + 1 + 64) 512 of
             0 -> 0
             n -> 512 - n

binDigitToWord32 :: Char -> Word32
binDigitToWord32 '0' = 0
binDigitToWord32 '1' = 1
binDigitToWord32  _  = error "binDigitToWord32: shouldn't happen"

binStringToWord32 :: String -> Word32
binStringToWord32 = foldl step 0
  where step acc x = acc * 2 + binDigitToWord32 x

-- | Show a Word32 as hex string with 8 characters
showFullHex :: Word32 -> String
showFullHex w32 = zeroFillUpTo 8 (showHex w32 "")

-- Prepare Input for Hashing
-- ============================================================================

prepMsg :: String -> [[Word32]]
prepMsg msg = chunked_str
  where
    -- Length of the message in bits
    l = 8 * length msg

    -- Step 1: Do the padding
    padded_str = concat [ stringToBinaryString msg  -- Message in binary (l)
                        , "1"                       -- Bit "1"           (1)
                        , nZeros (comp_k l)         -- Zero filling      (k)
                        , intToBinaryString l ]     -- Length in binary  (64)

    -- Step 2: Split in chunks of 512 bits, each containing 16 32bit words
    chunked_str = [ [ binStringToWord32 s32 | s32 <- chunksOf 32 ch512]
                  | ch512 <- chunksOf 512 padded_str ]

-- Program Constants
-- ============================================================================

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

-- The Logical Auxiliary Functions
-- ============================================================================

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

-- Main Hashing Function
-- ============================================================================

sha256 :: [[Word32]] {- Input (prepped): each element contains 16 Word32s (==512 bits) -}
       -> (Word32,Word32,Word32,Word32,Word32,Word32,Word32,Word32) {- Digest: 256 bits / 8 Word32s -}
sha256 mn = loop mn (h01,h02,h03,h04,h05,h06,h07,h08)
  where
    -- | SHA256 Compression Function
    comp_fn :: [Word32]                                                   -- current input block
            -> (Word32,Word32,Word32,Word32,Word32,Word32,Word32,Word32)  -- current a..h
            -> (Word32,Word32,Word32,Word32,Word32,Word32,Word32,Word32)  -- resulting a..h
    comp_fn block hs = aux 0 hs
      where
        w :: Array Int Word32
        w = array (0,63) ((zip [0..15] block)
                          ++ [ (j, (lS1 (w!(j-2)))
                                   + (w!(j-7))
                                   + (lS0 (w!(j-15)))
                                   + (w!(j-16)))
                             | j <- [16..63] ])

        aux :: Int                                                        -- iteration (j)
            -> (Word32,Word32,Word32,Word32,Word32,Word32,Word32,Word32)  -- current a..h
            -> (Word32,Word32,Word32,Word32,Word32,Word32,Word32,Word32)  -- resulting a..h
        aux j (a,b,c,d,e,f,g,h)
          | j > 63    = (a,b,c,d,e,f,g,h)
          | otherwise = let t1 = h + (uS1 e) + (ch e f g) + (k64 ! j) + (w ! j) in
                        let t2 = (uS0 a) + (maj a b c) in
                        aux (j+1) (t1+t2,a,b,c,d+t1,e,f,g)

    -- | Main Loop
    loop :: [[Word32]]                                                 -- blocks of 512
         -> (Word32,Word32,Word32,Word32,Word32,Word32,Word32,Word32)  -- current H1..H8
         -> (Word32,Word32,Word32,Word32,Word32,Word32,Word32,Word32)  -- resulting H1..H8
    loop [] (h1,h2,h3,h4,h5,h6,h7,h8)
      = (h1,h2,h3,h4,h5,h6,h7,h8)
    loop (block:blocks) (h1,h2,h3,h4,h5,h6,h7,h8)
      = loop blocks (a+h1,b+h2,c+h3,d+h4,e+h5,f+h6,g+h7,h+h8)
      where (a,b,c,d,e,f,g,h) = comp_fn block (h1,h2,h3,h4,h5,h6,h7,h8)

-- Interface and Tests
-- ============================================================================

sha256_bytestring :: BS.ByteString -> String
sha256_bytestring msg = concat
                      [ showFullHex h1, showFullHex h2, showFullHex h3, showFullHex h4
                      , showFullHex h5, showFullHex h6, showFullHex h7, showFullHex h8 ]
  where
    (h1,h2,h3,h4,h5,h6,h7,h8) = sha256 $ padChunkMsg msg

-- TESTING
sha256_string :: String -> String
sha256_string msg = concat
                      [ showFullHex h1, showFullHex h2, showFullHex h3, showFullHex h4
                      , showFullHex h5, showFullHex h6, showFullHex h7, showFullHex h8 ]
  where
    (h1,h2,h3,h4,h5,h6,h7,h8) = sha256 $ prepMsg msg

-- TESTING
sha256_io_string :: String -> IO ()
sha256_io_string msg = putStrLn (sha256_string msg)

sha256_io_bytestring :: BS.ByteString -> IO ()
sha256_io_bytestring msg = putStrLn (sha256_bytestring msg)

-- TESTING
sha256_test :: IO ()
sha256_test = do
  let b1 = sha256_string "abc"
             == "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
  let b2 = sha256_string "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
             == "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1"
  if (b1 && b2)
    then putStrLn "tests passed!"
    else putStrLn (unwords ["failure:",show b1,show b2])


-- Commented Trash
-- ============================================================================

-- R^n || shiftR :: a -> Int -> a infixl 8
-- S^n || rotateR :: a -> Int -> a infixl 8

{-
import Data.ByteString
  uncons :: ByteString -> Maybe (Word8, ByteString)
  readFile :: FilePath -> IO ByteString
  writeFile :: FilePath -> ByteString -> IO ()
  length :: ByteString -> Int
-}

-- > showIntAtBase 2 intToDigit 0x6a09e667 ""
-- "1101010000010011110011001100111"

-- > mapM (\l -> mapM (putStrLn . (\w32 -> showFullHex w32)) l) (prepMsg "abc")


-- partitionPaddedMsg :: String -> [[String]]
-- partitionPaddedMsg s = [ chunksOf 32 ch512 | ch512 <- chunksOf 512 s ]

-- int32BE :: Int32 -> Builder
-- toLazyByteString :: Builder -> ByteString
-- unpack :: ByteString -> [Word8]

-- import Data.ByteString.Lazy (unpack)
--
-- encodeWord16 :: Word16 -> [Word8]
-- encodeWord16 = unpack . toLazyByteString . word16LE

-- putStrLn $ showHex 12 "" -- prints "c"
-- putStrLn $ showIntAtBase 2 intToDigit 12 "" -- prints "1100"

-- compute_stuff s =
--
--   where
--     l  = 8 * length s
--     b  = 1
--     lb = 64

-- a  b  c  -- in char
-- ======= ord =======
-- 61 62 63 -- in hex
-- 97 98 99 -- in dec

-- import Data.ByteString.Builder(int32BE, toLazyByteString)
-- import Data.ByteString.Lazy (unpack)

-- $ echo -n abc | sha256sum
-- ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad  -
-- $ echo -n "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq" | sha256sum
-- 248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1  -

-- "abc"
--   ==> ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad

-- "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
--   ==> 248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1

-- if you want to try a string, not a file, do
--
-- $ echo -n abc | sha256sum
-- ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad  -
-- $ echo -n "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq" | sha256sum
-- 248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1  -

-- "abc"
--   ==> ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad

-- "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
--   ==> 248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1

-- a = array (1,100) ((1,1) : [(i, i * a!(i-1)) | i <- [2..100]])


-- | Padding for SHA256 hashing (TODO: Move to SHA256.hs)
padBS :: BS.ByteString -> BS.ByteString
padBS bs = BS.concat [ bs                                           -- message
                     , BS.pack (0x80 : replicate (no_bytes-1) 0x00) -- 1 0..0
                     , BS.pack (T.w64_bytes (fromIntegral len)) ]     -- 64
  where
    -- Length of message
    len :: Int
    len = 8 * BS.length bs

    -- Number of bytes to add (1 0..0)
    no_bytes | (d,r) <- (comp_k len+1) `quotRem` 8
             = if r /= 0 then error "padBS: what??" else d

-- | Partition a message into chunks of 512 bits, .....
chunkMsg :: BS.ByteString -> [[Word32]]
chunkMsg = map (map T.bs_w32 . T.chunksOf 4) . T.chunksOf 64

padChunkMsg :: BS.ByteString -> [[Word32]]
padChunkMsg = chunkMsg . padBS

-- Main Hashing Function
-- ============================================================================


-- TODO: Change the rep of Word512 to an array of Word32

buildW :: T.Word512 -> Array Int Word32
buildW (T.W512 w0 w1 w2 w3 w4 w5 w6 w7 w8 w9 w10 w11 w12 w13 w14 w15) = w
  where
    block = [w0,w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15]
    w     = array (0,63) ((zip [0..15] block)
                          ++ [ (j, lS1 (w!(j-2)) + (w!(j-7)) + lS0 (w!(j-15)) + (w!(j-16)))
                             | j <- [16..63] ])

sha256w :: [T.Word512] -> T.Word256
sha256w mn = loop mn (T.W256 h01 h02 h03 h04 h05 h06 h07 h08)
  where
    -- | SHA256 Compression Function
    comp_fn :: T.Word512 -> T.Word256 -> T.Word256
    comp_fn block hs = aux 0 hs
      where
        w :: Array Int Word32
        w = buildW block

        aux :: Int -> T.Word256 -> T.Word256
        aux j (T.W256 a b c d e f g h)
          | j > 63    = T.W256 a b c d e f g h
          | otherwise = let t1 = h + (uS1 e) + (ch e f g) + (k64 ! j) + (w ! j) in
                        let t2 = (uS0 a) + (maj a b c) in
                        aux (j+1) (T.W256 (t1+t2) a b c (d+t1) e f g)

    -- | Main Loop
    loop :: [T.Word512] -> T.Word256 -> T.Word256
    loop [] (T.W256 h1 h2 h3 h4 h5 h6 h7 h8)
      = T.W256 h1 h2 h3 h4 h5 h6 h7 h8
    loop (block:blocks) (T.W256 h1 h2 h3 h4 h5 h6 h7 h8)
      = loop blocks (T.W256 (a+h1) (b+h2) (c+h3) (d+h4) (e+h5) (f+h6) (g+h7) (h+h8))
      where (T.W256 a b c d e f g h) = comp_fn block (T.W256 h1 h2 h3 h4 h5 h6 h7 h8)


{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-} -- TODO: Remove

module Encodings.Base58Check where

-- ENCODING SOURCE:
--   https://en.bitcoin.it/wiki/Base58Check_encoding#Creating_a_Base58Check_string

import Data.Array
import Data.Words
import Hash.SHA256
import qualified Data.Map as M
import Control.Monad (guard)
import Encodings.Hex
import Debug.Trace (trace)

-- * Encoding into Base58Check
-- ----------------------------------------------------------------------------

-- TODO: Is it a good idea to have it as a string? A bytestring better maybe?
encodeTable :: (Ix i, Integral i) => Array i Char
encodeTable = listArray (0,57) "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

-- | Base58 encoding
encodeBase58 :: (Ix i, Integral i) => i -> String
encodeBase58 m | m == 0    = "1"      -- special case
               | otherwise = aux m ""
  where
    aux n acc | n > 0, (d,r) <- quotRem n 58 = aux d ((encodeTable ! r) : acc)
              | otherwise = acc

-- | Count the leading zero bytes
leadingEmptyWords :: Bytes a => a -> Int
leadingEmptyWords x = aux 0 (toBytes x)
  where
    aux acc (b:bs) | b == 0 = aux (acc+1) bs
    aux acc _               = acc

test1 = encodeBase58Check 0 (readHex "031bab84e687e36514eeaf5a017c30d32c1f59dd4ea6629da7970ca374513dd006" :: ByteString)

test2 = encodeBase58Check 42 (readHex "031bab84e687e36514eeaf5a017c30d32c1f59dd4ea6629da7970ca374513dd006" :: ByteString)

-- | Base58Check encoding (String)
encodeBase58Check :: Bytes a => Word8 {- version byte -} -> a {- payload -} -> String
encodeBase58Check version_byte payload
  = {-trace (show (doubleSHA256 version_payload))-} result
  where
    -- Step 1: Concatenate the version byte and the payload
    version_payload :: ByteString
    version_payload = fromBytes (version_byte : toBytes payload)

    -- Step 2: Double-hash (1) and keep only the first four bytes
    checksum :: [Word8]
    checksum = take 4 $ toBytes $ doubleSHA256 version_payload

    -- Step 3: Complete message in bytestring format
    msg_bytestring :: ByteString
    msg_bytestring = fromBytes (toBytes version_payload ++ checksum)

    -- Step 4: Convert to Base58 and drop leading '1's (== 0)
    msg_base58 :: String
    msg_base58 = dropWhile (=='1') $ encodeBase58 $ bsToInteger msg_bytestring

    -- Step 5: Create the prefix of '1's
    prefix_ones :: String
    prefix_ones = replicate (leadingEmptyWords msg_bytestring) '1'

    -- Step 6: Concatenate (4) and (5) to get the final result
    result :: String
    result = prefix_ones ++ msg_base58

-- * Testing
-- ----------------------------------------------------------------------------

-- IT WORKS
test_base58_100000 :: Bool
test_base58_100000 = all checkOne [1..100000]
  where
    checkOne :: Integer -> Bool
    checkOne i = case decodeBase58 (encodeBase58 i) of
                   Nothing -> error "invalid char!" -- False
                   Just x  -> if x == i then True
                                        else error (show i)


test_base58check :: Bool
test_base58check = all checkOne [ (vb, msg) | vb <- [1..255], msg <- [1,11..3000] ]
  where
    checkOne :: (Integer, Integer) -> Bool
    checkOne (vb,msg) = let enc = encodeBase58Check (fromInteger vb) (integerToBS msg)
                        in  case decodeBase58Check enc of
                              Nothing -> False
                              Just (vb',msg',_chk) -> vb'  == fromInteger vb
                                                   && msg' == integerToBS msg

test_integer_bytestring :: Bool
test_integer_bytestring = all checkOne [1..100000]
  where
    checkOne :: Integer -> Bool
    checkOne i = i == bsToInteger (integerToBS i)

test_print_decode :: String -> IO ()
test_print_decode input = case decodeBase58Check input of
  Nothing -> putStrLn "This failed :/"
  Just (vb,msg :: ByteString,checksum) -> do
    putStrLn ("checksum : " ++ show checksum)
    putStrLn ("payload  : " ++ showHex msg)
    putStrLn ("version  : " ++ show vb)

-- decodeBase58Check :: Bytes a => String -> Maybe (Word8, a, Word32)
-- encodeBase58Check :: Bytes a => Word8 {- version byte -} -> a {- payload -} -> String

-- * Decoding from Base58Check
-- ----------------------------------------------------------------------------

decodeMap :: Integral a => M.Map Char a
decodeMap = M.fromList
          $ zip "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
                [0..57]

-- | Base58 decoding
decodeBase58 :: Integral a => String -> Maybe a
decodeBase58 = aux 0
  where
    aux :: Integral b => b -> String -> Maybe b
    aux acc []     = Just acc
    aux acc (c:cs) | Just i <- M.lookup c decodeMap = aux (acc*58 + i) cs
                   | otherwise                      = Nothing

-- | Base58Check decoding (String)
decodeBase58Check :: Bytes a => String -> Maybe (Word8, a, Word32) -- (version byte, data, checksum)
decodeBase58Check input = do
  -- Step 1: Split it in leading zeros and the rest
  let (leading, rest) = span (=='1') input

  -- Step 2: Decode the second part of the input
  i <- decodeBase58 rest

  -- Step 3: Turn the decoded part into bytes
  let bytes = integerToBytes i

  -- Step 4: Append as many empty bytes as there are leading '1's
  let complete = replicate (length leading) 0x00 ++ bytes

  -- Step 5: Make sure the length is at least five (1 version, 4, checksum)
  guard (length complete >= 5)

  -- Step 6: Partition the message
  let (part1,part2,part3) = partitionBytes complete

  -- Step 7: Check the checksum
  let (front, checksum) = splitAt (length complete - 4) complete
  guard ((take 4 $ toBytes $ doubleSHA256 $ fromBytes front) == checksum)
  -- TODO TODO TODO TODO

  return (part1, fromBytes part2, fromBytes part3)

-- TODO: I HATE this function too :P
partitionBytes :: [a] -> (a, [a], [a])
partitionBytes xs = let (front, checksum) = splitAt (length xs - 4) xs
                    in  (head front, tail front, checksum)


-- TODO: George HATES the Enum class.

-- || -- | Base58 encoding
-- || encodeBase58 :: (Ix i, Integral i) => i -> String
-- || encodeBase58 m | m == 0    = "1"      -- special case
-- ||                | otherwise = aux m ""
-- ||   where
-- ||     aux n acc | n > 0, (d,r) <- quotRem n 58 = aux d ((encodeTable ! r) : acc)
-- ||               | otherwise = reverse acc
-- ||

-- integerToBytes :: Integer -> [Word8]
-- integerToBS :: Integer -> BS.ByteString


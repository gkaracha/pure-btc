{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-} -- TODO: Remove

module Encoding.Base58Check where

-- ENCODING SOURCE:
--   https://en.bitcoin.it/wiki/Base58Check_encoding#Creating_a_Base58Check_string

import Data.Words
import Hash.SHA256 (doubleSHA256)
import Control.Monad (guard)
import Encoding.Base58 (encodeBase58, decodeBase58)
import Encoding.Hex (showHex)

-- * Encoding into Base58Check
-- ----------------------------------------------------------------------------

-- | Base58Check encoding (String)
encodeBase58Check :: Bytes a => Word8 {- version byte -} -> a {- payload -} -> String
encodeBase58Check version_byte payload = result
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

-- | Count the leading zero bytes
leadingEmptyWords :: Bytes a => a -> Int
leadingEmptyWords x = aux 0 (toBytes x)
  where
    aux acc (b:bs) | b == 0 = aux (acc+1) bs
    aux acc _               = acc

-- * Decoding from Base58Check
-- ----------------------------------------------------------------------------

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

  return (part1, fromBytes part2, fromBytes part3)

-- TODO: I HATE this function too :P
partitionBytes :: [a] -> (a, [a], [a])
partitionBytes xs = let (front, checksum) = splitAt (length xs - 4) xs
                    in  (head front, tail front, checksum)

-- * Testing
-- ----------------------------------------------------------------------------

-- | https://github.com/libbitcoin/libbitcoin-explorer/wiki/bx-base58check-decode
-- | https://bitcoin.stackexchange.com/questions/5671/how-do-you-perform-double-sha-256-encoding
-- | https://en.bitcoin.it/wiki/Base58Check_encoding#Creating_a_Base58Check_string
-- | https://github.com/joeblackwaslike/base58check
-- | https://en.bitcoin.it/wiki/List_of_address_prefixes
-- | https://en.bitcoin.it/wiki/Base58Check_encoding

-- || test_print_decode "173RKgkk7fMbYUYBGyyAHeZ6rwfKRMn17h7DtGsmpEdab8TV6UB"
-- || checksum : 1020266843
-- || payload  : 031bab84e687e36514eeaf5a017c30d32c1f59dd4ea6629da7970ca374513dd006
-- || version  : 0
-- ||
-- || test_print_decode "7DTXS6pY6a98XH2oQTZUbbd1Z7P4NzkJqfraixprPutXQVTkwBGw"
-- || checksum : 3840642601
-- || payload  : 031bab84e687e36514eeaf5a017c30d32c1f59dd4ea6629da7970ca374513dd006
-- || version  : 42
-- ||
-- || test_print_decode "12ANjYr7zPnxRdZfnmC2e6jjHDpBY"
-- || checksum : 530125139
-- || payload  : 5361746f736869204e616b616d6f746f
-- || version  : 0

test_print_decode :: String -> IO ()
test_print_decode input = case decodeBase58Check input of
  Nothing -> putStrLn "This failed :/"
  Just (vb,msg :: ByteString,checksum) -> do
    putStrLn ("checksum : " ++ show (revWord32 checksum))
    putStrLn ("payload  : " ++ showHex msg)
    putStrLn ("version  : " ++ show vb)


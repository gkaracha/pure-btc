{-# OPTIONS_GHC -Wall #-}

module Encodings.Base58Check where

-- import Data.List
import Data.Array

-- || class (Real a, Enum a) => Integral a where
-- ||   quot :: a -> a -> a
-- ||   rem :: a -> a -> a
-- ||   div :: a -> a -> a
-- ||   mod :: a -> a -> a
-- ||   quotRem :: a -> a -> (a, a)
-- ||   divMod :: a -> a -> (a, a)
-- ||   toInteger :: a -> Integer


code_string :: String
code_string = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

-- listArray :: Ix i => (i, i) -> [e] -> Array i e

encodeTable :: (Ix i, Integral i) => Array i Char -- Is it a good idea to have it as a string? A bytestring better maybe?
encodeTable = listArray (0,57) "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

-- || decodeTable ::
-- || decodeTable

encodeBase58 :: (Ix i, Integral i) => i -> String
encodeBase58 m | m == 0    = "1"      -- special case
               | otherwise = aux m ""
  where
    aux n acc | n > 0, (d,r) <- quotRem n 58 = aux d ((encodeTable ! r) : acc)
              | otherwise = reverse acc

--    code_string = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
--    x = convert_bytes_to_big_integer(hash_result)
--
--    output_string = ""
--
--    while(x > 0)
--        {
--            (x, remainder) = divide(x, 58)
--            output_string.append(code_string[remainder])
--        }
--
--    repeat(number_of_leading_zero_bytes_in_hash)
--        {
--        output_string.append(code_string[0]);
--        }
--
--    output_string.reverse();


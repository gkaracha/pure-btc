{-# OPTIONS_GHC -Wall #-}

module Main where

import Data.Word.Word128 ()
import Data.Word.Word160 ()
import Data.Word.Word256 ()
import Data.Word.Word512 ()
import Data.Words         ()

import Encoding.Hex         ()
import Encoding.Base58      ()
import Encoding.Base58Check ()
import Encoding.WIF         ()

import Util.ByteString ()
import Util.Error      ()
import Util.List       ()
import Util.Numeric    ()

import Hash.SHA256    ()
import Hash.RIPEMD160 ()
import Hash.ECM       ()
import Hash.HASH160   ()

import Key.Private ()
import Key.Public  ()
import Key.Address ()

main :: IO ()
main = putStrLn "Remember to load all files to make sure they compile!"

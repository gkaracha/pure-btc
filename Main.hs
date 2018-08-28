{-# OPTIONS_GHC -Wall #-}

module Main where

import Data.Word.Word128 ()
import Data.Word.Word160 ()
import Data.Word.Word256 ()
import Data.Word.Word512 ()
import Data.Words         ()

import Encodings.Hex         ()
import Encodings.Base58      ()
import Encodings.Base58Check ()
import Encodings.WIF         ()

import Utils.ByteString ()
import Utils.Error      ()
import Utils.List       ()
import Utils.Numeric    ()

import Hash.SHA256    ()
import Hash.RIPEMD160 ()
import Hash.ECM       ()
import Hash.HASH160   ()

import Key.Private ()
import Key.Public  ()
import Key.Address ()

main :: IO ()
main = putStrLn "Remember to load all files to make sure they compile!"

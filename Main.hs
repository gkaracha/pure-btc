{-# OPTIONS_GHC -Wall #-}

module Main where

import Data.Word.Partition  ()
import Data.Word.Word128    ()
import Data.Word.Word160    ()
import Data.Word.Word256    ()
import Data.Word.Word512    ()

import Encoding.Base58Check ()
import Encoding.Base58      ()
import Encoding.Hex         ()
import Encoding.WIF         () -- not currently used / undone

import Hash.ECM             ()
import Hash.HASH160         ()
import Hash.RIPEMD160       ()
import Hash.SHA256          ()

import Key.Address          () -- no checks yet
import Key.Private          () -- no checks yet
import Key.Public           () -- no checks yet

import Util.ByteString      ()
import Util.Error           ()
import Util.List            ()
import Util.Numeric         ()

import PrettyPrint          () -- not currently used

import Script.Eval          () -- undone
import Script.Parser        () -- undone
import Script.Syntax        () -- undone

main :: IO ()
main = putStrLn "Remember to load all files to make sure they compile!"

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

module CommandLine (
  getOpt
) where

import BasicPrelude
import Data.Text (pack)
import Filesystem.Path.CurrentOS (decodeString)
import System.Console.CmdArgs hiding (name)

data Option  = Option {
               word :: String,
               depth :: Int,
               output :: String,
               wdir :: String
             } deriving (Data, Show, Typeable)

option :: Option
option = Option {
    word   = def &= typ "STRING",
    depth  = def &= typ "INTEGER",
    output = def &= typ "FILE",
    wdir   = def &= typ "DIR"
} &= program "grep-tree"

getOpt :: IO (Text, Int, FilePath, FilePath)
getOpt = do
    opts <- cmdArgs option
    return (
             pack $ word opts,
             depth opts,
             decodeString $ output opts,
             decodeString $ wdir opts
           )

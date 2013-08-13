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
import System.Console.CmdArgs

data Option  = Option {
               depth       :: Maybe Int,
               sourcelines :: Maybe Int,
               output      :: Maybe String,
               wdir        :: Maybe String,
               word        :: String
             } deriving (Data, Show, Typeable)

option :: Option
option = Option {
    depth        = def &= typ "INTEGER" &= help "Grep depth",
    sourcelines  = def &= typ "INTEGER" &= help "The size of source to show in tooltip",
    output       = def &= typ "FILE"    &= help "Json file name",
    wdir         = def &= typ "DIR"     &= help "Directory where to grep",
    word         = def &= argPos 0
} &= program "grep-tree"

getOpt :: IO (Text, Int, Int, Maybe FilePath, FilePath)
getOpt = do
    let default_depth = 3
    let default_sourcelines = 24
    opts <- cmdArgs option
    return (
             pack $ word opts,
             default_depth `fromMaybe` (depth opts),
             default_sourcelines `fromMaybe` (sourcelines opts),
             decodeString <$> output opts,
             decodeString ("." `fromMaybe` (wdir opts))
           )

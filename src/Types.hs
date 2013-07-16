{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE DeriveDataTypeable #-}
{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}

module Types where

import Data.Aeson
import Data.Text hiding (map, head, concatMap)
import Data.Maybe
import System.Console.CmdArgs

import Prelude

type Lines = [Text]
type Line = Text
type Word = Text
type Dir = FilePath

data Tree = Tree {
          word :: Maybe Text,
          fname :: FilePath,
          lnum :: Int,
          corners :: [Corner],
          children :: [Tree]
          } deriving (Show, Eq)

data OutputTree = OutputTree {
                  name :: Text,
                  wordO :: Maybe Text,
                  fnameO :: FilePath,
                  lnumO :: Int,
                  cornersO :: Text,
                  childrenO :: [OutputTree]
                  } deriving (Show, Eq)

data Option  = Option {
               wordOpt :: String,
               depthOpt :: Int,
               outputOpt :: String,
               wdirOpt :: String
             } deriving (Data, Show, Typeable)

option :: Option
option = Option {
  wordOpt   = def &= typ "STRING",
  depthOpt  = def &= typ "INTEGER",
  outputOpt = def &= typ "FILE",
  wdirOpt   = def &= typ "DIR"
} &= program "grep-tree"

-- | 種類*キーワード*原文*Grep結果
data Corner = Corner Kind Text deriving (Show, Eq) 

data Kind = Class Word | Module Word | ClassMethod (Maybe Word) | Method (Maybe Word) | Block | If | Other | End deriving (Show, Eq)

instance ToJSON OutputTree where
    toJSON (OutputTree n w fn ln cs chds) = object [
                                                "name" .= n,
                                                "word" .= w,
                                                "fname" .= fn,
                                                "lnum" .= ln, 
                                                "corners" .= cs,
                                                "children" .= toJSON chds
                                                ]

instance ToJSON Corner where
    toJSON (Corner kind orig) = object [
                                "kind" .= show kind,
                                "original_text" .= orig
                                ]

showCorner :: Corner -> Text
showCorner (Corner (Class w) _) = ':' `cons` w
showCorner (Corner (Module w) _) = '_' `cons` w
showCorner (Corner (ClassMethod (Just w)) _) = '.' `cons` w
showCorner (Corner (ClassMethod Nothing) _) = ".()"
showCorner (Corner (Method (Just w)) _) = '#' `cons` w
showCorner (Corner (Method Nothing) _) = "#()"
showCorner (Corner Block _) = "~"
showCorner (Corner If _) = "\'"
showCorner (Corner Other _) = "@"
showCorner (Corner End _) = "/"

showCorners :: [Corner] -> Text
showCorners = intercalate "" . map showCorner

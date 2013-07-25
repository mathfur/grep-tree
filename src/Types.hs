{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}

module Types where

import Prelude hiding (lookup, map, lines)
import Data.Aeson
import Data.Text hiding (map, head, concatMap)
import Data.Text.Encoding (decodeUtf8')
import qualified Data.ByteString.Char8 as BC
import qualified Data.List as L
import qualified Data.Map as M
import System.Console.CmdArgs hiding (name)
import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Control.Monad.IO.Class

type Lines = [Text]
type Line = Text
type Word = Text
type Dir = FilePath

data Tree = Tree {
          primary_word :: Maybe Text,
          search_word :: Text,
          fname :: FilePath,
          lnum :: Int,
          corners :: [Corner],
          children :: [Tree]
          } deriving (Show, Eq)

data OutputTree = OutputTree {
                  name :: Text,
                  primary_wordO :: Maybe Text,
                  search_wordO :: Text,
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

getOpt :: IO (String, Int, FilePath, FilePath)
getOpt = do
    opts <- cmdArgs option
    let w = wordOpt opts
    let depth = depthOpt opts
    let output = outputOpt opts
    let wdir = wdirOpt opts
    return (w, depth, output, wdir)

-- | 種類*キーワード*原文*Grep結果
data Corner = Corner Kind Text deriving (Show, Eq)

data Kind = Class Word | Module Word | ClassMethod (Maybe Word) | Method (Maybe Word) | Block | If | Other | End deriving (Show, Eq)

type TreeGenerator a = StateT (M.Map (CacheKey, FilePath) [Text]) (WriterT [FilePath] IO) a
data CacheKey = GitGrepCache Word | ReadFileCache deriving (Show, Eq, Ord)

readFileThroughCache :: CacheKey -> FilePath -> TreeGenerator [Text]
readFileThroughCache key path = do
  ls <- readCache key path
  case ls of
    Just inner -> return inner
    Nothing -> do
      cnt_or_error <- decodeUtf8' <$> (liftIO $ BC.readFile path)
      case cnt_or_error of
        Right cnt -> do
          let new_ls = lines cnt
          writeCache key path new_ls
          return new_ls
        Left _ -> do
          lift $ tell [path]
          return []

writeCache :: CacheKey -> FilePath -> [Text] -> TreeGenerator ()
writeCache key path ls = do
    pairs <- get
    put $ M.insert (key, path) ls pairs

readCache :: CacheKey -> FilePath -> TreeGenerator (Maybe [Text])
readCache key path = do
    pairs <- get
    return $ M.lookup (key, path) pairs

instance ToJSON OutputTree where
    toJSON (OutputTree {..}) = object [
                                      "name" .= name,
                                      "primary_word" .= primary_wordO,
                                      "search_word" .= search_wordO,
                                      "fname" .= fnameO,
                                      "lnum" .= lnumO,
                                      "corners" .= cornersO,
                                      "children" .= toJSON childrenO
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
showCorners = intercalate "" . L.map showCorner

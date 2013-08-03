{-# LANGUAGE NoImplicitPrelude #-} 
{-# LANGUAGE OverloadedStrings #-} 
{-# OPTIONS_GHC -Wall #-}

module Helpers where

import Prelude (init, tail, (.), fst, (==), or, Int, Char, Bool, String, otherwise, (||), ($))
import Types
import Parser
import Data.Text (Text, singleton, length, concatMap, span, unpack)
import Data.List (all, isPrefixOf, break)
import Data.Maybe
import System.FilePath (FilePath, takeBaseName)

initOrBlank :: [a] -> [a]
initOrBlank [] = []
initOrBlank xs = init xs

tailOrBlank :: [a] -> [a]
tailOrBlank [] = []
tailOrBlank xs = tail xs

-- | 行の先頭空白数を得る
indentLevel :: Line -> Int
indentLevel = length . concatMap expandTab . fst . span isBlank
  where
    expandTab :: Char -> Text
    expandTab '\t' = "    "
    expandTab c = singleton c

isBlankLine :: Line -> Bool
isBlankLine = all isBlank . unpack

isBlank :: Char -> Bool
isBlank c = (or [c == ' ', c == '\t'])

toCorner :: Text -> Corner
toCorner orig = Corner (getKind orig) orig

dropFirstUnderScore :: String -> String
dropFirstUnderScore ('_':cs) = cs
dropFirstUnderScore cs = cs

fnameToRailsDirectory :: FilePath -> Maybe RailsDirectory
fnameToRailsDirectory fn
  | "app/controllers/" `isPrefixOf` fn    = Just RailsController
  | "app/models/" `isPrefixOf` fn         = Just RailsModel
  | "app/views/" `isPrefixOf` fn          = Just RailsView
  | "app/helper/" `isPrefixOf` fn         = Just RailsHelper
  | "lib/" `isPrefixOf` fn                = Just RailsLib
  | "vendor/" `isPrefixOf` fn             = Just RailsVendor
  | "config/" `isPrefixOf` fn             = Just RailsConfig
  | "public/javascripts/" `isPrefixOf` fn = Just RailsJs
  | "publis/stylesheets/" `isPrefixOf` fn = Just RailsStyleSheet
  | "db/" `isPrefixOf` fn                 = Just RailsDb
  | "test/" `isPrefixOf` fn || "spec/" `isPrefixOf` fn = Just RailsTest
  | otherwise                             = Nothing

-- |
--
-- >>> takeBaseName' "/foo/bar.baz.txt"
-- "bar"
takeBaseName' :: FilePath -> String
takeBaseName' path = fst $ break (== '.') $ takeBaseName path

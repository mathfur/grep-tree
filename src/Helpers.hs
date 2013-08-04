{-# LANGUAGE NoImplicitPrelude #-} 
{-# LANGUAGE OverloadedStrings #-} 
{-# OPTIONS_GHC -Wall #-}

module Helpers where

import Prelude hiding (length, concatMap, span)
import Types
import Parser
import Data.Text (Text, singleton, length, concatMap, span, unpack)
import Data.List (isPrefixOf)
import System.FilePath (takeBaseName)

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

-- |
--
-- >>> slice 2 3 [0, 1, 2, 3, 4]
-- [2,3]
--
-- >>> slice (-2) 3 [0, 1, 2, 3, 4]
-- [0,1,2,3]
--
-- >>> slice 3 6 [0, 1, 2, 3, 4]
-- [3,4]
slice :: Int -> Int -> [a] -> [a]
slice start_ind end_ind ls = take len $ drop start_ind' $ ls
  where
    start_ind' = start_ind `max` 0
    len = (end_ind - start_ind' + 1) `max` 0

isCommentOnlyLine :: Line -> Bool
isCommentOnlyLine l = all (\c -> c == ' ' || c == '\t') $ fst $ break (== '#') $ (unpack l)

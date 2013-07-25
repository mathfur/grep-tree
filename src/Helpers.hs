{-# LANGUAGE NoImplicitPrelude #-} 
{-# LANGUAGE OverloadedStrings #-} 
{-# OPTIONS_GHC -Wall #-}

module Helpers where

import Prelude (init, tail, (.), fst, (==), or, Int, Char, Bool)
import Types
import Parser
import Data.Text (Text, singleton, length, concatMap, span, unpack)
import Data.List (all)

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

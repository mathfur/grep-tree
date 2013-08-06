{-# LANGUAGE NoImplicitPrelude #-} 
{-# LANGUAGE OverloadedStrings #-} 
{-# OPTIONS_GHC -Wall #-}

module Helpers where

import BasicPrelude hiding (Word)
import qualified Data.Aeson as DA
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Text as T
import Filesystem.Path.CurrentOS hiding (encode)

initOrBlank :: [a] -> [a]
initOrBlank [] = []
initOrBlank xs = init xs

tailOrBlank :: [a] -> [a]
tailOrBlank [] = []
tailOrBlank xs = tail xs

isBlank :: Char -> Bool
isBlank c = (or [c == ' ', c == '\t'])

-- | 行の先頭空白数を得る
indentLevel :: Text -> Int
indentLevel = T.length . T.concatMap expandTab . fst . T.span isBlank
  where
    expandTab :: Char -> Text
    expandTab '\t' = "    "
    expandTab c = T.singleton c

isBlankLine :: Text -> Bool
isBlankLine = all isBlank . T.unpack

dropFirstUnderScore :: Text -> Text
dropFirstUnderScore = T.pack . f . T.unpack
  where
    f ('_':cs) = cs
    f cs = cs

toText' :: FilePath -> Text
toText' fn = case (toText fn) of
               Left _ -> error $ "toText is failed"
               Right fn' -> fn'

isCommentOnlyLine :: Text -> Bool
isCommentOnlyLine l = all (\c -> c == ' ' || c == '\t') $ fst $ break (== '#') $ (T.unpack l)

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

jsonToText :: (DA.ToJSON a) => a -> Text
jsonToText = T.pack . BLC.unpack . DA.encode . DA.toJSON

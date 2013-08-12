{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}

module Types where

import BasicPrelude hiding (Word)
import Data.Aeson
import Data.Text hiding (map, head, concatMap, intercalate, isPrefixOf)
import qualified Data.List as L
import Control.Lens (makeLenses)
import Filesystem.Path.CurrentOS

type Lines = [Text]
type Line = Text
type Word = Text
type Pattern = Text
type ErrMsg = Text
type Dir = FilePath
type NamedRoute = Text
type ControllerName = Text
type ActionName = Text

data Objective = RegexpObjective (Maybe Pattern) Pattern
               | WordObjective Word
               | NoObjective
               deriving (Show, Eq)

data Kind = RbClass Word
          | RbModule Word
          | RbClassMethod (Maybe Word)
          | RbMethod (Maybe Word)
          | RbBlock
          | RbIf
          | RbEnd
          | JsFunc (Maybe Word)
          | JsEnd
          | Other
          | CurrentLine
          deriving (Show, Eq)

-- | 種類*キーワード*原文*Grep結果
data Corner = Corner Kind Text deriving (Show, Eq)

data Tree = Tree {
          _primary_word :: Maybe Text,
          _search_word :: Text,
          _fname :: FilePath,
          _lnum :: Int,
          _corners :: [Corner],
          _is_action :: Bool,
          _is_filter_def :: Bool,
          _around_text :: [(Int, Text)],
          _children :: [Tree]
          } deriving (Show, Eq)

makeLenses ''Tree

data OutputTree = OutputTree {
                  name :: Text,
                  primary_wordO :: Maybe Text,
                  search_wordO :: Text,
                  fnameO :: Text,
                  lnumO :: Int,
                  cornersO :: Text,
                  is_actionO :: Bool,
                  is_filter_defO :: Bool,
                  rails_directory :: Maybe RailsDirectory,
                  around_textO :: [(Int, Text)],
                  childrenO :: [OutputTree]
                  } deriving (Show, Eq)

data AllTree = AllTree {
                 nameA :: Text,
                 primary_wordA :: Maybe Text,
                 cornersA :: Text,
                 childrenA :: [OutputTree],
                 err_filesA :: [Text]
               }

data RailsRoute = RailsRoute {
                    httpMethod :: Text,
                    routeMap :: Text,
                    controllerName :: ControllerName, -- e.g. users_controller
                    actionName :: ActionName
                  } deriving (Show, Eq)

data RailsDirectory = RailsController
                    | RailsModel
                    | RailsView
                    | RailsHelper
                    | RailsPartialView
                    | RailsLib
                    | RailsVendor
                    | RailsConfig
                    | RailsJs
                    | RailsStyleSheet
                    | RailsDb
                    | RailsTest
                    deriving (Show, Eq)

------------------------------------------------------------------
-- | ToJSON instance

instance ToJSON AllTree where
    toJSON (AllTree {..}) = object [
                              "name" .= nameA,
                              "primary_word" .= primary_wordA,
                              "corners" .= cornersA,
                              "children" .= childrenA,
                              "error_files" .= err_filesA
                            ]

instance ToJSON OutputTree where
    toJSON (OutputTree {..}) = object [
                                      "name" .= name,
                                      "primary_word" .= primary_wordO,
                                      "search_word" .= search_wordO,
                                      "fname" .= fnameO,
                                      "lnum" .= lnumO,
                                      "is_action" .= is_actionO,
                                      "is_filter_def" .= is_filter_defO,
                                      "rails_directory" .= rails_directory,
                                      "corners" .= cornersO,
                                      "around_text" .= around_textO,
                                      "children" .= toJSON childrenO
                                      ]

instance ToJSON Corner where
    toJSON (Corner kind orig) = object [
                                "kind" .= show kind,
                                "original_text" .= orig
                                ]

instance ToJSON FilePath where
    toJSON = toJSON . toText

instance ToJSON RailsDirectory where
    toJSON RailsController  = "controller"
    toJSON RailsModel       = "model"
    toJSON RailsView        = "view"
    toJSON RailsHelper      = "helper"
    toJSON RailsPartialView = "partial_view"
    toJSON RailsLib         = "lib"
    toJSON RailsVendor      = "vendor"
    toJSON RailsConfig      = "config"
    toJSON RailsJs          = "js"
    toJSON RailsStyleSheet  = "stylesheet"
    toJSON RailsDb          = "db"
    toJSON RailsTest        = "test"

------------------------------------------------------------------
-- | helpers

showCorner :: Corner -> Text
showCorner (Corner (RbClass w) _) = ':' `cons` w
showCorner (Corner (RbModule w) _) = '_' `cons` w
showCorner (Corner (RbClassMethod (Just w)) _) = '.' `cons` w
showCorner (Corner (RbClassMethod Nothing) _) = ".()"
showCorner (Corner (RbMethod (Just w)) _) = '#' `cons` w
showCorner (Corner (RbMethod Nothing) _) = "#()"
showCorner (Corner RbBlock _) = "B"
showCorner (Corner RbIf _) = "|"
showCorner (Corner Other _) = "^"
showCorner (Corner RbEnd _) = ">"
showCorner (Corner (JsFunc (Just w)) _) = '#' `cons` w
showCorner (Corner (JsFunc Nothing) _) = "#()"
showCorner (Corner JsEnd _) = ">"
showCorner (Corner CurrentLine _) = "@"

showCorners :: [Corner] -> Text
showCorners = intercalate "" . L.map showCorner

haveWord :: Corner -> Bool
haveWord = isJust . getWord

getWord :: Corner -> Maybe Word
getWord (Corner (RbClass w) _) = Just w
getWord (Corner (RbModule w) _) = Just w
getWord (Corner (RbClassMethod w) _) = w
getWord (Corner (RbMethod w) _) = w
getWord (Corner RbBlock _) = Nothing
getWord (Corner RbIf _) = Nothing
getWord (Corner RbEnd _) = Nothing
getWord (Corner (JsFunc w) _) = w
getWord (Corner JsEnd _) = Nothing
getWord (Corner CurrentLine _) = Nothing
getWord (Corner Other _) = Nothing

fnameToRailsDirectory :: FilePath -> Maybe RailsDirectory
fnameToRailsDirectory path
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
    where
      fn = encodeString path


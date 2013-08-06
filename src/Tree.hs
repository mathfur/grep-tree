{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

module Tree where

import Prelude (($), String)
import Data.Text hiding (map, head, reverse, tail, take, last)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BLC

import Types

generateJSON :: Word -> [OutputTree] -> [Text] -> String
generateJSON w output_trees err_files = BLC.unpack $ encode $ toJSON $ object [
                                                                         "name" .= ("main" :: Text),
                                                                         "primary_word" .= w,
                                                                         "corners" .= w,
                                                                         "children" .= output_trees,
                                                                         "error_files" .= err_files
                                                                       ]

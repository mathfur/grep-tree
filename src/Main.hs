{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import BasicPrelude hiding (Word, liftIO, lift)

import CommandLine
import TreeGenerator

main :: IO ()
main = do
    (search_word, depth, sourcelines, output_fname, wdir) <- getOpt
    (jsn, err_files) <- searchAndGetTree wdir depth sourcelines search_word
    case output_fname of
      Just fname -> writeFile fname jsn
      Nothing -> putStrLn jsn

    when (0 < length err_files) $ do
      putStrLn "Error at:"
      forM_ err_files print

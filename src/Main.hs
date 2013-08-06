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
    (search_word, depth, output_fname, wdir) <- getOpt
    (jsn, err_files) <- searchAndGetTree wdir depth search_word
    writeFile output_fname jsn

    when (0 < length err_files) $ do
      putStrLn "Error at:"
      forM_ err_files print

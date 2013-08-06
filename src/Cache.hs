{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

module Cache (
  readCache,
  writeCache,
  CacheKey(CommandCache, FileCache),
  CachedIO
) where

import BasicPrelude hiding (Word, liftIO, lift)
import qualified Data.Map as M
import Control.Monad.Trans.State

type CachedIO = StateT (M.Map CacheKey [Text]) IO
data CacheKey = CommandCache Text [Text] (Maybe FilePath)
              | FileCache FilePath
              deriving (Show, Eq, Ord)

writeCache :: CacheKey -> [Text] -> CachedIO ()
writeCache key ls = do
    pairs <- get
    put $ M.insert key ls pairs

readCache :: CacheKey -> CachedIO (Maybe [Text])
readCache key = do
    pairs <- get
    return $ M.lookup key pairs

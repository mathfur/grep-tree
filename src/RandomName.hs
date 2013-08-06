{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

module RandomName (
  randomName
) where

import BasicPrelude hiding (Word, liftIO, lift)
import Data.Text (pack)
import Control.Monad.Random hiding (split)

randomName :: (RandomGen g) => Rand g Text
randomName = pack <$> (replicateM 20 $ getRandomR ('a', 'z'))

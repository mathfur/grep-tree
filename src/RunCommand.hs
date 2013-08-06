{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

module RunCommand (
  execute,
  executeWithCache
) where

import BasicPrelude hiding (Word, liftIO, lift)
import Data.Text (pack, unpack)
import Control.Monad.IO.Class
import System.IO hiding (FilePath, readFile, writeFile, putStrLn)
import System.Process
import Filesystem.Path.CurrentOS

import Cache

executeWithCache :: Text -> [Text] -> Maybe FilePath -> CachedIO (Either Text [Text])
executeWithCache command_name args wdir = do
  ls <- readCache key
  case ls of
    Just ls_ -> return $ Right ls_
    Nothing -> do
      result <- liftIO $ execute command_name args wdir
      case result of
        Left err -> do
          return $ Left err
        Right result' -> do
          writeCache key result'
          return $ Right result'
    where
      key = CommandCache command_name args wdir

execute :: Text -> [Text] -> Maybe FilePath -> IO (Either Text [Text])
execute command_name args wdir = do
   (_,hdl,_,_) <- runInteractiveProcess (unpack command_name) (map unpack args) ((unpack . fromRight . toText) <$> wdir) Nothing
   hSetBinaryMode hdl False
   hSetBuffering hdl LineBuffering
   catch (hGetLineToEOF hdl <* (hClose hdl)) exception_handler
     where
       exception_handler :: SomeException -> IO (Either Text [Text])
       exception_handler e = return $ Left $ "Failed at: "
                                            `mappend` command_name `mappend` " "
                                            `mappend` (show args) `mappend` " "
                                            `mappend` (show wdir) `mappend` " : "
                                            `mappend` (show e)
       fromRight x = case x of
                      Left _ -> error "fromRight is failed"
                      Right y -> y

hGetLineToEOF :: Handle -> IO (Either Text [Text])
hGetLineToEOF hdl = do
  is_eof <- (hIsEOF hdl)
  if is_eof then return $ Right []
            else do
              new_line <- hGetLine hdl
              following <- hGetLineToEOF hdl
              case following of
                Right ls -> return $ Right ((pack new_line) : ls)
                Left err -> return $ Left err

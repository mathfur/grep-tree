{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Prelude hiding (length, break, lines, concatMap, readFile, writeFile, rem)
import Data.Text hiding (map, head, reverse, tail, take, last)
import Data.Text.IO
import qualified Data.ByteString.Lazy.Char8 as BLC
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Control.Applicative
import Data.Aeson
import Data.Maybe
import qualified Data.List as L
import qualified Data.Map as M
import System.Process
import System.IO hiding (hGetLine, readFile, writeFile)
import Control.Monad.Random hiding (split)
import System.FilePath

import Types
import Helpers

main :: IO ()
main = do
    (w, depth, output, wdir) <- getOpt
    ((tree, _), err_files) <- runWriterT $ runStateT (wordToTree wdir depth (pack w)) (M.fromList [])
    output_trees <- evalRandIO $ mapM toOutputTree tree
    let jsn = BLC.unpack $ encode $ toJSON $ object ["name" .= ("main" :: Text), "children" .= output_trees, "error_files" .= err_files]
    writeFile output $ pack jsn

toOutputTree :: (RandomGen g) => Tree -> Rand g OutputTree
toOutputTree (Tree {..}) = do
    random_name <- randomName
    OutputTree random_name primary_word search_word fname lnum corner_str <$> mapM toOutputTree children
      where
        randomName :: (RandomGen g) => Rand g Text
        randomName = pack <$> (replicateM 20 $ getRandomR ('a', 'z'))

        corner_str = showCorners corners

wordToTree :: FilePath -> Int -> Word -> TreeGenerator [Tree]
wordToTree _ 0 _ = return []
wordToTree wdir depth search_wd = do
    results <- grepCommand wdir search_wd
    catMaybes <$> (forM results (\line -> do
        let (path, ln, _) = parseGrepResult line
        cnt <- readFileThroughCache ReadFileCache (wdir </> path)
        let (primary_corner, all_corners) = getCorners cnt (ln - 1)
        case primary_corner of
          Nothing -> (return $ Just $ Tree {
                                             primary_word = Nothing,
                                             search_word = search_wd,
                                             fname = path,
                                             lnum = ln,
                                             corners = all_corners,
                                             children = []
                                           })
          Just w_ -> case getPrimaryWord w_ of
            Nothing -> (return $ Just $ Tree {
                                               primary_word = Nothing,
                                               search_word = search_wd,
                                               fname = path,
                                               lnum = ln,
                                               corners = all_corners,
                                               children = []
                                             })
            Just w__ -> (do
              ts <- wordToTree wdir (depth - 1) w__
              return $ Just $ Tree {
                                     primary_word = Just w__,
                                     search_word = search_wd,
                                     fname = path,
                                     lnum = ln,
                                     corners = all_corners,
                                     children = ts
                                   })))

grepCommand :: FilePath -> Word -> TreeGenerator [Text]
grepCommand wdir w = do
   ls <- readCache (GitGrepCache w) wdir
   case ls of
     Just ls_ -> return ls_
     Nothing -> do
       (_,hdl,_,_) <- liftIO $ runInteractiveProcess "git" ["grep", "-n", unpack w] (Just wdir) Nothing
       liftIO $ hSetBinaryMode hdl False
       liftIO $ hSetBuffering hdl LineBuffering
       new_ls <- (liftIO (hGetLineToEOF hdl <* (liftIO $ hClose hdl)))
       writeCache (GitGrepCache w) wdir new_ls
       return new_ls

hGetLineToEOF :: Handle -> IO [Text]
hGetLineToEOF hdl = do
  is_eof <- (liftIO $ hIsEOF hdl)
  if is_eof then return []
            else ((:) <$> (liftIO $ hGetLine hdl) <*> hGetLineToEOF hdl)

-- |
-- >>> parseGrepResult (pack "foo/bar.hs:123: foo bar")
-- ("foo/bar.hs",123," foo bar")
parseGrepResult :: Text -> (FilePath, Int, Text)
parseGrepResult line
  | L.length ss < 2 = error "fail to parse the result of grep"
  | otherwise     = (fn, num, matched_string)
    where
      ss = split (== ':') $ line
      fn = unpack $ ss !! 0
      num = read $ unpack $ ss !! 1
      matched_string = ss !! 2

getPrimaryWord :: Corner -> Maybe Word
getPrimaryWord (Corner (Class _) _) = Nothing
getPrimaryWord (Corner (Module _) _) = Nothing
getPrimaryWord c = getWord c

getWord :: Corner -> Maybe Word
getWord (Corner (Class w) _) = Just w
getWord (Corner (Module w) _) = Just w
getWord (Corner (ClassMethod w) _) = w
getWord (Corner (Method w) _) = w
getWord (Corner Block _) = Nothing
getWord (Corner If _) = Nothing
getWord (Corner Other _) = Nothing
getWord (Corner End _) = Nothing

haveWord :: Corner -> Bool
haveWord = isJust . getWord

-- | ある行の上にある角と下にある角を取得する
-- 戻り値: (Intで指定した関数の最初の行と思われるCorner, 全Corner)
--
-- >>> getCorners [] 0
-- (Nothing,[])
--
-- >>> getCorners [pack "def foo", pack "  print 123", pack "end"] 1
-- (Just (Corner (Method (Just "foo")) "def foo"),[Corner (Method (Just "foo")) "def foo",Corner End "end"])
--
-- >>> getCorners (map pack ["def bar", "  def foo", "    print 123", "  end", "end"]) 2
-- (Just (Corner (Method (Just "foo")) "  def foo"),[Corner (Method (Just "bar")) "def bar",Corner (Method (Just "foo")) "  def foo",Corner End "  end",Corner End "end"])
--
-- >>> getCorners (map pack ["def foo(s)", "  print s", "end", "", "def bar(src)", "  foo(src)", "end", "", "puts bar()"]) 1
-- (Just (Corner (Method (Just "foo")) "def foo(s)"),[Corner (Method (Just "foo")) "def foo(s)",Corner End "end"])
--
-- >>> getCorners (map pack ["def foo(s)", "  print s", "end", "", "def bar(src)", "  foo(src)", "end", "", "puts bar()"]) 4
-- (Nothing,[])
--
-- >>> getCorners (map pack ["def foo(s)", "  print s", "end", "", "def bar(src)", "  foo(src)", "end", "", "puts bar()"]) 5
-- (Just (Corner (Method (Just "bar")) "def bar(src)"),[Corner (Method (Just "bar")) "def bar(src)",Corner End "end"])
--
-- >>> getCorners (map pack ["module Foo", "  def foo(s)", "    print s", "  end", "", "  def bar(src)", "    foo(src)", "  end", "end", "", "puts bar('13:45')"]) 6
-- (Just (Corner (Method (Just "bar")) "  def bar(src)"),[Corner (Module "Foo") "module Foo",Corner (Method (Just "bar")) "  def bar(src)",Corner End "  end",Corner End "end"])
--
-- >>> getCorners (map pack ["module Foo", "  def foo(s)", "    print s", "  end", "", "  def bar(src)", "    foo(src)", "  end", "end", "", "puts bar('13:45')"]) 10
-- (Nothing,[])
getCorners :: [Line] -> Int -> (Maybe Corner, [Corner])
getCorners ls num = (primary_corner, all_corners)
    where
      up_corners = (getUpCorners (L.take (num + 1) ls))
      down_corners = (getDownCorners ((L.drop num) ls))
      all_corners = up_corners ++ down_corners

      primary_corner = if (L.length up_corners == 0) then Nothing else (L.find haveWord $ reverse up_corners)

-- | ある行の上にある角を取得
getUpCorners :: [Line] -> [Corner]
getUpCorners = reverse . getDownCorners . reverse

-- | ある行の下にある角を取得
getDownCorners :: [Line] -> [Corner]
getDownCorners = map (\(_, _, orig) -> toCorner orig) . tailOrBlank . getCornerLines

-- | 角行のみ得る(行番号, インデントレベル, 原文).
--
-- >>> getCornerLines []
-- []
--
-- >>> getCornerLines $ map pack ["def foo"]
-- [(0,0,"def foo")]
--
-- >>> getCornerLines $ map pack ["  def foo"]
-- [(0,2,"  def foo")]
--
-- >>> getCornerLines $ map pack ["  print 123", "end"]
-- [(0,2,"  print 123"),(1,0,"end")]
--
-- >>> getCornerLines $ map pack ["    print 123", "  end", "end"]
-- [(0,4,"    print 123"),(1,2,"  end"),(2,0,"end")]
--
-- >>> getCornerLines $ map pack ["  print 123", "end", "print 456"]
-- [(0,2,"  print 123"),(1,0,"end")]
--
-- >>> getCornerLines $ map pack ["  print 123", "  print 456", "end"]
-- [(0,2,"  print 123"),(2,0,"end")]
--
-- >>> getCornerLines $ map pack ["  print 123", "  if true", "  end", "  end"]
-- [(0,2,"  print 123")]
--
-- >>> getCornerLines $ map pack ["  print 123", "  if true", "  end", "end"]
-- [(0,2,"  print 123"),(3,0,"end")]
--
-- >>> getCornerLines $ map pack ["  print s", "end", "", "def bar(src)", "  foo(src)", "end", "", "puts bar()"]
-- [(0,2,"  print s"),(1,0,"end")]
getCornerLines :: [Line] -> [(Int, Int, Line)]
getCornerLines [] = []
getCornerLines ls = (0, indentLevel fstline, fstline) : incLNum (getCornerLines rem)
  where
    firstIndentLevel = indentLevel $ head ls
    (fstls@(fstline:_), rem) = L.break (\l -> (firstIndentLevel > indentLevel l) && (not $ isBlankLine l)) ls
    incLNum :: [(Int, Int, Line)] -> [(Int, Int, Line)]
    incLNum = map (\(i, idt, s) -> (i + L.length fstls, idt, s))


{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Prelude hiding (length, break, lines, concatMap, readFile, writeFile, rem, catch, putStrLn)
import Data.Text hiding (map, head, reverse, tail, take, last)
import Data.Text.IO
import qualified Data.ByteString.Lazy.Char8 as BLC
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Control.Applicative
import Control.Exception
import Data.Aeson
import Data.Maybe
import qualified Data.List as L
import qualified Data.Map as M
import System.Process
import System.IO hiding (hGetLine, readFile, writeFile, putStrLn)
import Control.Monad.Random hiding (split)
import System.FilePath

import Types
import Helpers
import Parser

main :: IO ()
main = do
    (w, depth, output, wdir) <- getOpt
    ((tree, _), err_files) <- runWriterT $ runStateT (wordToTree wdir depth (pack w)) (M.fromList [])
    output_trees <- evalRandIO $ mapM toOutputTree tree
    let jsn = BLC.unpack $ encode $ toJSON $ object [
                                                    "name" .= ("main" :: Text),
                                                    "primary_word" .= pack w,
                                                    "corners" .= pack w,
                                                    "children" .= output_trees,
                                                    "error_files" .= err_files
                                                    ]
    writeFile output $ pack jsn
    putStrLn "Error at:"
    forM_ err_files print

toOutputTree :: (RandomGen g) => Tree -> Rand g OutputTree
toOutputTree (Tree {..}) = do
    random_name <- randomName
    children' <- mapM toOutputTree children
    return $ OutputTree {
               name = random_name,
               primary_wordO = primary_word,
               search_wordO = search_word,
               fnameO = fname,
               rails_directory = fnameToRailsDirectory fname,
               lnumO = lnum,
               is_actionO = is_action,
               cornersO = corner_str,
               around_textO = around_text,
               childrenO = children'
             }
      where
        randomName :: (RandomGen g) => Rand g Text
        randomName = pack <$> (replicateM 20 $ getRandomR ('a', 'z'))

        corner_str = showCorners corners

wordToTree :: FilePath -> Int -> Pattern -> TreeGenerator [Tree]
wordToTree _ 0 _ = return []
wordToTree wdir depth pattern = do
    results <- grepCommand wdir pattern
    let results' = catMaybes $ map parseGrepResult results
    catMaybes <$> (forM results' (\(path, ln, _) -> do
        let abs_path = wdir </> path
        cnt <- readFileThroughCache abs_path
        let (objective, all_corners) = getPrimaryWord cnt (ln - 1) abs_path
        let text_range = 8
        around_text <- slice (ln - text_range) (ln + text_range) <$> L.zip [1..] <$> readFileThroughCache path
        case objective of
            NoObjective -> (return $ Just $ Tree {
                                               primary_word = Nothing,
                                               search_word = pattern,
                                               fname = path,
                                               lnum = ln,
                                               is_action = False,
                                               corners = all_corners,
                                               around_text = around_text,
                                               children = []
                                             })
            WordObjective next_word -> (do
              ts <- wordToTree wdir (depth - 1) next_word
              is_action <- isAction wdir path next_word
              return $ Just $ Tree {
                                     primary_word = Just next_word,
                                     search_word = pattern,
                                     fname = path,
                                     lnum = ln,
                                     corners = all_corners,
                                     around_text = around_text,
                                     is_action = is_action,
                                     children = ts
                                   })
            RegexpObjective next_pattern -> (do
              ts <- wordToTree wdir (depth - 1) next_pattern
              return $ Just $ Tree {
                                     primary_word = Just next_pattern,
                                     search_word = pattern,
                                     fname = path,
                                     lnum = ln,
                                     corners = all_corners,
                                     around_text = around_text,
                                     is_action = False,
                                     children = ts
                                   })))

isAction :: FilePath -> FilePath -> Word -> TreeGenerator Bool
isAction wdir path w = do
    cnt <- readFileThroughCache (wdir </> "rake_routes")
    let routes = parseRakeRouteLine cnt
    return $ (flip L.any) routes (\route ->
                                 ((controllerName route) :: Text) `append` "_controller" == ((pack $ takeBaseName' path))
                                 &&
                                 (((actionName route) :: Text) == w))

grepCommand :: FilePath -> Word -> TreeGenerator [Text]
grepCommand wdir w = do
   ls <- readCache (GitGrepCache w) wdir
   case ls of
     Just ls_ -> return ls_
     Nothing -> do
       (_,hdl,_,_) <- liftIO $ runInteractiveProcess "git" ["grep", "-n", "-w", unpack w] (Just wdir) Nothing
       liftIO $ hSetBinaryMode hdl False
       liftIO $ hSetBuffering hdl LineBuffering
       new_ls <- liftIO $ catch (liftIO $ hGetLineToEOF hdl <* (liftIO $ hClose hdl)) (exception_handler w)
       case new_ls of
         Left msg -> do
           lift $ tell [unpack msg]
           return []
         Right success_word -> do
           writeCache (GitGrepCache w) wdir success_word
           return success_word
     where
       exception_handler :: Text -> SomeException -> IO (Either Text [Text])
       exception_handler search_word e = return (Left $ "searching is failed at " `append` search_word `append` ": " `append` (pack $ show e))

hGetLineToEOF :: Handle -> IO (Either Text [Text])
hGetLineToEOF hdl = do
  is_eof <- (liftIO $ hIsEOF hdl)
  if is_eof then return $ Right []
            else do
              new_line <- liftIO $ hGetLine hdl
              following <- hGetLineToEOF hdl
              case following of
                Right ls -> return $ Right (new_line : ls)
                Left err -> return $ Left err

-- |
-- >>> parseGrepResult (pack "foo/bar.hs:123: foo bar")
-- Just ("foo/bar.hs",123," foo bar")
--
-- >>> parseGrepResult (pack "Binary file public/images/foo.jpg matches")
-- Nothing
parseGrepResult :: Text -> Maybe (FilePath, Int, Text)
parseGrepResult line
  | L.length ss < 2 = if ("Binary file" `isPrefixOf` line) then Nothing
                                                           else error ("fail to parse the result of grep: " ++ unpack line)
  | otherwise     = Just (fn, num, matched_string)
    where
      ss = split (== ':') $ line
      fn = unpack $ ss !! 0
      num = read $ unpack $ ss !! 1
      matched_string = ss !! 2

-- |
--
-- >>> getPrimaryWord (map pack ["<div>", "  <%= foo %>", "</div>"]) 1 "/foo/bar.html.erb"
-- (RegexpObjective "render.*bar",[])
--
-- >>> getPrimaryWord (map pack ["def foo", "  print 123", "end"]) 1 "/foo/bar.rb"
-- (WordObjective "foo",[Corner (RbMethod (Just "foo")) "def foo",Corner CurrentLine "  print 123",Corner RbEnd "end"])
--
-- >>> getPrimaryWord (map pack ["foo: function(e){", "  console.log(e)", "}"]) 1 "/foo/bar.js"
-- (WordObjective "foo",[Corner (JsFunc (Just "foo")) "foo: function(e){",Corner CurrentLine "  console.log(e)",Corner JsEnd "}"])
--
getPrimaryWord :: [Line] -> Int -> FilePath -> (Objective, [Corner])
getPrimaryWord ls dpt path
   | not (hasExtension path)              = (NoObjective, [])
   | (ext == ".erb") || (ext == ".rhtml") = (RegexpObjective $ "render.*" `append` basename, [])
   | ext == ".js"                         = case primary_corner of
                                              Just x -> (wordToObj $ getWord x, all_corners)
                                              Nothing -> (NoObjective, all_corners)
   | (ext == ".rb") || (ext == ".rake")   = case primary_corner of
                                              Just (Corner (RbClass _) _) -> (NoObjective, all_corners)
                                              Just (Corner (RbModule _) _) -> (NoObjective, all_corners)
                                              Nothing -> (NoObjective, all_corners)
                                              Just x -> (wordToObj $ getWord x, all_corners)
   | otherwise                            = (NoObjective, [])
     where
       (primary_corner, all_corners) = getCorners ls dpt
       ext = pack $ takeExtension path
       basename = pack $ dropFirstUnderScore $ takeBaseName' path

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

wordToObj :: Maybe Word -> Objective
wordToObj Nothing = NoObjective
wordToObj (Just w) = WordObjective w

haveWord :: Corner -> Bool
haveWord = isJust . getWord

-- | ある行の上にある角と下にある角を取得する
-- 戻り値: (Intで指定した関数の最初の行と思われるCorner, 全Corner)
--
-- >>> getCorners [] 0
-- (Nothing,[])
--
-- >>> getCorners [pack "def foo", pack "  print 123", pack "end"] 1
-- (Just (Corner (RbMethod (Just "foo")) "def foo"),[Corner (RbMethod (Just "foo")) "def foo",Corner CurrentLine "  print 123",Corner RbEnd "end"])
--
-- >>> getCorners (map pack ["def bar", "  def foo", "    print 123", "  end", "end"]) 2
-- (Just (Corner (RbMethod (Just "foo")) "  def foo"),[Corner (RbMethod (Just "bar")) "def bar",Corner (RbMethod (Just "foo")) "  def foo",Corner CurrentLine "    print 123",Corner RbEnd "  end",Corner RbEnd "end"])
--
-- >>> getCorners (map pack ["def foo(s)", "  print s", "end", "", "def bar(src)", "  foo(src)", "end", "", "puts bar()"]) 1
-- (Just (Corner (RbMethod (Just "foo")) "def foo(s)"),[Corner (RbMethod (Just "foo")) "def foo(s)",Corner CurrentLine "  print s",Corner RbEnd "end"])
--
-- >>> getCorners (map pack ["def foo(s)", "  print s", "end", "", "def bar(src)", "  foo(src)", "end", "", "puts bar()"]) 4
-- (Nothing,[Corner CurrentLine "def bar(src)"])
--
-- >>> getCorners (map pack ["def foo(s)", "  print s", "end", "", "def bar(src)", "  foo(src)", "end", "", "puts bar()"]) 5
-- (Just (Corner (RbMethod (Just "bar")) "def bar(src)"),[Corner (RbMethod (Just "bar")) "def bar(src)",Corner CurrentLine "  foo(src)",Corner RbEnd "end"])
--
-- >>> getCorners (map pack ["module Foo", "  def foo(s)", "    print s", "  end", "", "  def bar(src)", "    foo(src)", "  end", "end", "", "puts bar('13:45')"]) 6
-- (Just (Corner (RbMethod (Just "bar")) "  def bar(src)"),[Corner (RbModule "Foo") "module Foo",Corner (RbMethod (Just "bar")) "  def bar(src)",Corner CurrentLine "    foo(src)",Corner RbEnd "  end",Corner RbEnd "end"])
--
-- >>> getCorners (map pack ["module Foo", "  def foo(s)", "    print s", "  end", "", "  def bar(src)", "    foo(src)", "  end", "end", "", "puts bar('13:45')"]) 10
-- (Nothing,[Corner CurrentLine "puts bar('13:45')"])
getCorners :: [Line] -> Int -> (Maybe Corner, [Corner])
getCorners [] _ = (Nothing, [])
getCorners ls num = (primary_corner, all_corners)
    where
      up_corners = (getUpCorners (L.take (num + 1) ls))
      down_corners = (getDownCorners ((L.drop num) ls))
      current_corner = Corner CurrentLine (ls !! num)
      all_corners = up_corners ++ [current_corner] ++ down_corners

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
-- >>> getCornerLines $ map pack ["  print 123", "#foo", "end"]
-- [(0,2,"  print 123"),(2,0,"end")]
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
    (fstls@(fstline:_), rem) = L.break (\l -> (firstIndentLevel > indentLevel l && not (isCommentOnlyLine l)) && (not $ isBlankLine l)) ls
    incLNum :: [(Int, Int, Line)] -> [(Int, Int, Line)]
    incLNum = map (\(i, idt, s) -> (i + L.length fstls, idt, s))

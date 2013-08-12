{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

module TreeGenerator (
  searchAndGetTree
) where

import BasicPrelude hiding (Word, liftIO, lift)
import Data.Text.Encoding (decodeUtf8')
import Data.Text (unpack, split)
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as M
import Control.Lens hiding (children)
import Control.Monad.IO.Class
import Control.Monad.Random hiding (split)
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Filesystem.Path.CurrentOS
import qualified GHC.IO as GI

import Types
import Helpers
import Parser
import RandomName
import Corner
import RunCommand
import Cache

-- $setup
-- >>> import Data.Text (pack)

type TreeGenerator a = WriterT [Text] CachedIO a
 
searchAndGetTree :: FilePath -> Int -> Pattern -> IO (Text, [Text])
searchAndGetTree wdir depth pattern = do
    (trees, err_files) <- runTreeGenerator $ wordToTree wdir depth pattern Nothing
    output_trees <- convertToOutputTree trees
    let jsn = jsonToText $ AllTree {
                             nameA = "main",
                             primary_wordA = Just pattern,
                             cornersA = pattern,
                             childrenA = output_trees,
                             err_filesA = err_files
                           }
    return (jsn, err_files)

----------------------------------------------------------------------------

runTreeGenerator :: TreeGenerator [Tree] -> IO ([Tree], [Text])
runTreeGenerator x = fst <$> (runStateT (runWriterT x) (M.fromList []))

wordToTree :: FilePath -> Int -> Pattern -> Maybe Pattern -> TreeGenerator [Tree]
wordToTree _ 0 _ _ = return []
wordToTree wdir depth pattern fname_pattern = do
    (results, errs) <- lift $ grepCommand 300 wdir pattern
    tell errs
    let results' = filter (\(path, _, _) -> hitPattern fname_pattern path) $ catMaybes $ map parseGrepResult results
    catMaybes <$> (forM results' (\(path, ln, _) -> do
        let abs_path = wdir </> path
        cnt <- readFileThroughCache abs_path
        let (objective, objecive_lnum, all_corners) = getPrimaryWord cnt (ln - 1) abs_path
        let text_range = 12
        let source = slice (ln - text_range) (ln + text_range) $ zip [1..] cnt
        let current_line = cnt !! ln
        let is_filter_def_ = isFilterDefinition current_line
        let common_tree = defaultTree pattern path ln all_corners source
        case objective of
            NoObjective -> return $ Just common_tree
            WordObjective next_word -> (do
              is_action_ <- isAction wdir path next_word
              ts <- if (not is_action_ && not is_filter_def_) then wordToTree wdir (depth - 1) next_word Nothing
                                                              else return []
              let ts_without_me = case objecive_lnum of
                                    Nothing -> ts
                                    Just lnum_ -> filter (not . is_myself path (lnum_ + 1)) ts
              return $ Just ( common_tree&primary_word.~(Just next_word)&is_action.~is_action_&is_filter_def.~is_filter_def_&children.~ts_without_me) )
            RegexpObjective fname_pattern' next_pattern -> (do
              ts <- wordToTree wdir (depth - 1) next_pattern fname_pattern'
              return $ Just ( common_tree&primary_word.~(Just next_pattern)&children.~ts ))))

hitPattern :: Maybe Pattern -> FilePath -> Bool
hitPattern Nothing _       = True
hitPattern (Just pat) path = ((unpack $ "/" ++ pat ++ "_") `isPrefixOf` path')
                          || ((unpack $ "/" ++ pat ++ "/") `isPrefixOf` path')
  where
    path' = encodeString path

is_myself :: FilePath -> Int -> Tree -> Bool
is_myself path ln tree = (path == _fname tree) && (ln == _lnum tree)

convertToOutputTree :: [Tree] -> IO [OutputTree]
convertToOutputTree trees = evalRandIO $ mapM toOutputTree trees

toOutputTree :: (RandomGen g) => Tree -> Rand g OutputTree
toOutputTree (Tree {..}) = do
    random_name <- randomName
    children' <- mapM toOutputTree _children
    return $ OutputTree {
               name = random_name,
               primary_wordO = _primary_word,
               search_wordO = _search_word,
               fnameO = toText' _fname,
               rails_directory = fnameToRailsDirectory _fname,
               lnumO = _lnum,
               is_actionO = _is_action,
               is_filter_defO = _is_filter_def,
               cornersO = corner_str,
               around_textO = _around_text,
               childrenO = children'
             }
      where
        corner_str = showCorners _corners

-- |
-- >>> parseGrepResult (pack "foo/bar.hs:123: foo bar")
-- Just (FilePath "foo/bar.hs",123," foo bar")
--
-- >>> parseGrepResult (pack "Binary file public/images/foo.jpg matches")
-- Nothing
parseGrepResult :: Text -> Maybe (FilePath, Int, Text)
parseGrepResult line
  | length ss < 2 = if ("Binary file" `isPrefixOf` (unpack line)) then Nothing
                                                                    else error ("fail to parse the result of grep: " ++ unpack line)
  | otherwise     = Just (fn, num, matched_string)
    where
      ss = split (== ':') $ line
      fn = fromText $ ss !! 0
      num = read $ ss !! 1
      matched_string = ss !! 2

-- |
--
-- >>> getPrimaryWord (map pack ["<div>", "  <%= foo %>", "</div>"]) 1 (fromText $ pack "/foo/bar.html.erb")
-- (RegexpObjective (Just "/foo") "render.*bar",Nothing,[])
--
-- >>> getPrimaryWord (map pack ["def foo", "  print 123", "end"]) 1 (fromText $ pack "/foo/bar.rb")
-- (WordObjective "foo",Just 0,[Corner (RbMethod (Just "foo")) "def foo",Corner CurrentLine "  print 123",Corner RbEnd "end"])
--
-- >>> getPrimaryWord (map pack ["foo: function(e){", "  console.log(e)", "}"]) 1 (fromText $ pack "/foo/bar.js")
-- (WordObjective "foo",Just 0,[Corner (JsFunc (Just "foo")) "foo: function(e){",Corner CurrentLine "  console.log(e)",Corner JsEnd "}"])
--
getPrimaryWord :: [Line] -> Int -> FilePath -> (Objective, Maybe Int, [Corner])
getPrimaryWord ls dpt path = case (extension path) of
   Nothing -> (NoObjective, Nothing, [])
   Just x
     | (x == "erb") || (x == "rhtml") -> (RegexpObjective fname_pattern $ "render.*" `mappend` basename_val, Nothing, [])
     | (x == "js")                    -> case primary_corner of
                                           Just x' -> (wordToObj $ getWord x', primary_lnum, all_corners)
                                           Nothing -> (NoObjective, primary_lnum, all_corners)
     | (x == "rb") || (x == "rake")   -> case primary_corner of
                                           Just (Corner (RbClass _) _) -> (NoObjective, primary_lnum, all_corners)
                                           Just (Corner (RbModule _) _) -> (NoObjective, primary_lnum, all_corners)
                                           Nothing -> (NoObjective, primary_lnum, all_corners)
                                           Just x' -> (wordToObj $ getWord x', primary_lnum, all_corners)
     | otherwise                      -> (NoObjective, Nothing, [])
   where
     (primary_corner, primary_lnum, all_corners) = getCorners ls dpt
     basename_val = dropFirstUnderScore $ toText' $ basename path
     fname_pattern = Just $ ("/" :: Text) ++ ((toText' $ dirname path) :: Text)

isAction :: FilePath -> FilePath -> Word -> TreeGenerator Bool
isAction wdir path w = do
    cnt <- readFileThroughCache (wdir </> "rake_routes")
    let routes = parseRakeRouteLine cnt
    return $ (flip any) routes (\route ->
                                 ((((controllerName route) :: Text) `mappend` "_controller") == (toText' $ basename path))
                                 &&
                                 (((actionName route) :: Text) == w))

readFileThroughCache :: FilePath -> TreeGenerator [Text]
readFileThroughCache path = do
  ls <- lift $ readCache (FileCache path)
  case ls of
    Just inner -> return inner
    Nothing -> do
      case (toText path) of
        Right path' -> do
          cnt_or_error <- decodeUtf8' <$> (liftIO $ BC.readFile (unpack path' :: GI.FilePath))
          case cnt_or_error of
            Right cnt -> do
              let new_ls = lines cnt
              lift $ writeCache (FileCache path) new_ls
              return new_ls
            Left _ -> do
              tell [path']
              return []
        Left path' -> do
          tell [path']
          return []

wordToObj :: Maybe Word -> Objective
wordToObj Nothing = NoObjective
wordToObj (Just w) = WordObjective w

defaultTree :: Word -> FilePath -> Int -> [Corner] -> [(Int, Text)] -> Tree
defaultTree pattern path ln all_corners source
  = Tree {
      _primary_word = Nothing,
      _search_word = pattern,
      _fname = path,
      _lnum = ln,
      _corners = all_corners,
      _around_text = source,
      _is_action = False,
      _is_filter_def = False,
      _children = []
    }

----------------------------------------------------------------------------
-- | Command

grepCommand :: Int -> FilePath -> Word -> CachedIO ([Text], [ErrMsg])
grepCommand limit wdir w = do
  result <- executeWithCache "git" ["grep", "-n", "-w", w] (Just wdir)
  case result of
    Left err -> return ([], [err])
    Right result' -> do
      let limit_err = if (limit < length result') then ["git grep result of " `mappend` w `mappend` " is more than "
                                                        `mappend` (show limit) `mappend` ": "
                                                        `mappend` (show $ length result')
                                                        ]
                                                  else []
      return (take limit result', limit_err)

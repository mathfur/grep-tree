{-# LANGUAGE NoImplicitPrelude #-} 
{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

import Prelude hiding (length, break, lines, concatMap)
import Data.Text hiding (map, head, reverse, tail, take, last)
import qualified Data.ByteString.Lazy.Char8 as BLC
import Control.Lens hiding (children)
import Control.Monad
import Control.Applicative
import Data.Aeson
import Data.Maybe
import qualified Data.List as L
import System.Process
import System.IO
import Control.Monad.Random hiding (split)
import System.Console.CmdArgs

import Parser
import Types
import Helpers

main :: IO ()
main = do
    opts <- cmdArgs option
    let w = pack $ wordOpt opts
    let depth = depthOpt opts
    let output = outputOpt opts
    let wdir = wdirOpt opts
    tree <- wordToTree wdir depth w
    output_trees <- evalRandIO $ mapM toOutputTree tree
    let jsn = BLC.unpack $ encode $ toJSON $ output_trees
    writeFile output jsn
      where
        toOutputTree :: (RandomGen g) => Tree -> Rand g OutputTree
        toOutputTree (Tree w fn ln cns children) = do
            random_name <- randomName
            OutputTree random_name w fn ln corner_str <$> mapM toOutputTree children
              where
                randomName :: (RandomGen g) => Rand g Text
                randomName = pack <$> (replicateM 20 $ getRandomR ('a', 'z'))
        
                corner_str = showCorners cns

wordToTree :: FilePath -> Int -> Word -> IO [Tree]
wordToTree _ 0 _ = return []
wordToTree wdir depth w = do
    results <- grepCommand wdir w
    catMaybes <$> (forM results (\line -> do
        print line
        let (path, ln, orig) = parseGrepResult line
        cnt <- (lines . pack) <$> (readFile path)
        print path
        print ln
        let (main_corner, all_corners) = getCorners cnt (ln - 1)
        print main_corner
        case main_corner of
          Nothing -> print ">>" >> return Nothing
          Just w_ -> case getWord w_ of
            Nothing -> print w_ >> (return $ Just $ Tree {
                                         word = Nothing,
                                         fname = path,
                                         lnum = ln,
                                         corners = all_corners,
                                         children = []
                                       })
            Just w__ -> print w__ >> (do
              ts <- (wordToTree wdir (depth - 1)) w__
              return $ Just $ Tree {
                                     word = Just w__,
                                     fname = path,
                                     lnum = ln,
                                     corners = all_corners,
                                     children = ts
                                   })))

grepCommand :: FilePath -> Word -> IO [Text]
grepCommand wdir w = do
   -- TODO: dirが存在することを確認
   (_,hdl,_,_) <- runInteractiveProcess "git" ["grep", "-n", unpack w] (Just wdir) Nothing
   hSetBinaryMode hdl False
   hSetBuffering hdl LineBuffering
   hGetLineToEOF hdl <* hClose hdl

hGetLineToEOF :: Handle -> IO [Text]
hGetLineToEOF hdl = do
  is_eof <- hIsEOF hdl
  if is_eof then return []
            else ((:) <$> (pack <$> hGetLine hdl) <*> hGetLineToEOF hdl)

parseGrepResult :: Text -> (FilePath, Int, Text)
parseGrepResult line
  | L.length ss < 2 = error "fail to parse the result of grep"
  | otherwise     = (fname, num, matched_string)
    where
      ss = split (== ':') $ line
      fname = unpack $ ss !! 0
      num = read $ unpack $ ss !! 1
      matched_string = ss !! 2

getWord :: Corner -> Maybe Word
getWord (Corner (Class w) _) = Just w
getWord (Corner (Module w) _) = Just w
getWord (Corner (ClassMethod w) _) = w
getWord (Corner (Method w) _) = w
getWord (Corner Block _) = Nothing
getWord (Corner If _) = Nothing
getWord (Corner Other _) = Nothing
getWord (Corner End _) = Nothing

-- | ある行の上にある角と下にある角を取得する
-- 戻り値: (Intで指定した関数の最初の行と思われるCorner, 全Corner)
getCorners :: [Line] -> Int -> (Maybe Corner, [Corner])
getCorners lines num = (main_corner, all_corners)
    where
      up_corners = (getUpCorners (take (num + 1) lines))
      down_corners = (getDownCorners ((reverse . take (L.length lines - num + 1) . reverse) lines))
      all_corners = up_corners ++ down_corners

      main_corner = if (L.length up_corners == 0) then Nothing else (Just $ last up_corners)

-- | ある行の上にある角を取得
getUpCorners :: [Line] -> [Corner]
getUpCorners = reverse . getDownCorners . reverse

-- | ある行の下にある角を取得
getDownCorners :: [Line] -> [Corner]
getDownCorners = map (\(_, _, orig) -> toCorner orig) . tailOrBlank . getCornerLines

-- | 角行のみ得る(行番号, インデントレベル, 原文).
getCornerLines :: [Line] -> [(Int, Int, Line)]
getCornerLines [] = []
getCornerLines [line] = [(0, indentLevel line, line)]
getCornerLines (line:lines)
    | (indentLevel line == secondIndentLevel lines) = incLNum $ getCornerLines lines
    | otherwise                                     = (0, indentLevel line, line) : (incLNum $ getCornerLines lines)
  where
    incLNum :: [(Int, Int, Line)] -> [(Int, Int, Line)]
    incLNum = map (\(i, idt, s) -> (i+1, idt, s))
    secondIndentLevel :: [Line] -> Int
    secondIndentLevel = (\(_, k, _) -> k) . head . getCornerLines

-- extend0 :: Int -> GrepResult -> IO [GrepResult]
-- extend0 i (GrepResult dir j cs) = GrepResult dir j <$> cs'
--     where
--       cs' = mapM f3 cs
--         where
--           f3 :: Corner -> IO Corner
--           f3 (Corner k w t Nothing)  =  Corner k w         <$> Nothing
--           f3 (Corner k w t (Just r)) = (Corner k w . Just) <$> r'
--             where
--               r' = if i > 0 then extend0 (i - 1) r
--                             else getGrepResult w
-- 
-- -- | grepしてある行を基点とした角情報を得る
-- getGrepResult :: Word -> IO [GrepResult]
-- getGrepResult w num = do
--     pairs <- grepCommand w
--     forM pairs (\(fname, num) -> do
--       cnt <- readFile fname
--       let corners = getUpCornersAndDownCorners (lines cnt) num
--       return $ GrepResult fname num corners)

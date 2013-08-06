{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

module Corner (
  getCorners
) where

import BasicPrelude hiding (Word, liftIO, lift)

import Types
import Helpers
import Parser

-- $setup
-- >>> import Data.Text (pack)

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
      up_corners = (getUpCorners (take (num + 1) ls))
      down_corners = (getDownCorners ((drop num) ls))
      current_corner = Corner CurrentLine (ls !! num)
      all_corners = up_corners ++ [current_corner] ++ down_corners

      primary_corner = if (length up_corners == 0) then Nothing else (find haveWord $ reverse up_corners)

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
getCornerLines ls = (0, indentLevel fstline, fstline) : incLNum (getCornerLines rem')
  where
    firstIndentLevel = indentLevel $ head ls
    (fstls@(fstline:_), rem') = break (\l -> (firstIndentLevel > indentLevel l && not (isCommentOnlyLine l)) && (not $ isBlankLine l)) ls
    incLNum :: [(Int, Int, Line)] -> [(Int, Int, Line)]
    incLNum = map (\(i, idt, s) -> (i + length fstls, idt, s))

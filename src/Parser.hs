{-# LANGUAGE OverloadedStrings #-} 
{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Parser (
  getKind
) where

import Prelude
import Text.Peggy
import Data.Text hiding (map, head, concatMap)

import Types

[peggy|
identifier ::: Text
  = [a-zA-Z0-9_-]+ { pack $1 }

class_method_name ::: Text
  = 'self.' [a-zA-Z0-9_-]+ { pack $1 }

class :: Kind
  = [ \t]* "class" [ \t]* identifier [ \t]* { Class $3 }

module :: Kind
  = [ \t]* "module" [ \t]* identifier [ \t]* { Module $3 }

method :: Kind
  = [ \t]* "def" [ \t]* identifier { Method (Just $3) }
  / [ \t]* "define_method" { Method Nothing }

class_method :: Kind
  = [ \t]* "def" [ \t]* class_method_name { ClassMethod (Just $3) }

block :: Kind
  = (!"do" . )* "do" [ \t]* !. { Block }

end :: Kind
  = [ \t]* "end" [ \t]* { End }

other :: Kind
  = .* { Other }

line :: Kind
  = class
  / module
  / class_method
  / method
  / end
  / block
  / other
|]

-- |
-- >>> getKind (pack "module Foo")
-- Module "Foo"
--
-- >>> getKind (pack "def self.foo")
-- ClassMethod (Just "foo")
--
-- >>> getKind (pack "def bar")
-- Method (Just "bar")
--
-- >>> getKind (pack "define_method :bar do")
-- Method Nothing
getKind :: Text -> Kind
getKind input = fromRight $ parseString line "<stdin>" $ unpack input
  where
    fromRight (Right x) = x
    fromRight (Left _) = error "kind can not be parsed"

{-# LANGUAGE OverloadedStrings #-} 
{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts #-}

module Parser where

import Prelude
import Text.Peggy
import Data.Text hiding (map, head, concatMap)

import Types

[peggy|
class :: Kind
  = [ \t]* "class" [ \t]* [a-zA-Z0-9_-]+ [ \t]* { Class (pack $4) }

module :: Kind
  = [ \t]* "module" [ \t]* [a-zA-Z0-9_-]+ [ \t]* { Module (pack $4) }

method :: Kind
  = [ \t]* "def" [ \t]* [a-zA-Z0-9_-]+ [^a-zA-Z0-9_-] { Method (Just (pack $3)) }
  / [ \t]* "define_method" { Method Nothing }

block :: Kind
  = .* "do" [ \t]* { Block }

end :: Kind
  = [ \t]* "end" [ \t]* { End }

other :: Kind
  = .* { Other }

line :: Kind
  = class
  / module
  / method
  / block
  / end
  / other
|]

getKind :: Text -> Kind
getKind input = fromRight $ parseString line "<stdin>" $ unpack input
  where
    fromRight (Right x) = x
    fromRight (Left _) = error "kind can not be parsed"

toCorner :: Text -> Corner
toCorner orig = Corner (getKind orig) orig

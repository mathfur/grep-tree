{-# LANGUAGE OverloadedStrings #-}
{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Parser (
  getKind,
  parseRakeRouteLine
) where

import Prelude
import Text.Peggy
import Data.Either
import Data.Text hiding (map, head, concatMap, lines)

import Types

[peggy|
identifier ::: Text
  = [a-zA-Z0-9_-]+ { pack $1 }

-- ruby

rb_class_method_name ::: Text
  = 'self.' [a-zA-Z0-9_-]+ { pack $1 }

rb_class :: Kind
  = [ \t]* "class" [ \t]* identifier [ \t]* { RbClass $3 }

rb_module :: Kind
  = [ \t]* "module" [ \t]* identifier [ \t]* { RbModule $3 }

rb_method :: Kind
  = [ \t]* "def" [ \t]* identifier { RbMethod (Just $3) }
  / [ \t]* "define_method" { RbMethod Nothing }

rb_class_method :: Kind
  = [ \t]* "def" [ \t]* rb_class_method_name { RbClassMethod (Just $3) }

rb_block :: Kind
  = (!"do" . )* "do" [ \t]* !. { RbBlock }

rb_end :: Kind
  = [ \t]* "end" [ \t]* { RbEnd }

-- js

js_func :: Kind
  = [ \t]* "function" [ \t]* identifier { JsFunc (Just $3) }
  / [ \t]* identifier ':' [ \t]* 'function(' { JsFunc (Just $2) }

js_end :: Kind
  = [ \t]* "}" { JsEnd }

other :: Kind
  = .* { Other }

line :: Kind
  = rb_class
  / rb_module
  / rb_class_method
  / rb_method
  / rb_end
  / rb_block
  / js_func
  / js_end
  / other

-- route

http_method ::: Text
  = "GET" { pack "GET" }
  / "POST" { pack "POST" }
  / "PUT" { pack "PUT" }
  / "DELETE" { pack "DELETE" }

-- e.g. pages GET /pages(.:format) {:action=>"index", :controller=>"pages"}
route_line :: RailsRoute
  = identifier http_method [^ ]+ "{:action=>\"" identifier "\"," ":controller=>\"" identifier "\"}" { RailsRoute $1 $2 (pack $3) $5 $4 }
|]

-- |
-- >>> getKind (pack "module Foo")
-- RbModule "Foo"
--
-- >>> getKind (pack "def self.foo")
-- RbClassMethod (Just "foo")
--
-- >>> getKind (pack "def bar")
-- RbMethod (Just "bar")
--
-- >>> getKind (pack "  foo: function( arg ) {")
-- JsFunc (Just "foo")
--
-- >>> getKind (pack "function bar( arg1, arg2 ) {")
-- JsFunc (Just "bar")
--
-- >>> getKind (pack "  }")
-- JsEnd
--
-- >>> getKind (pack "define_method :bar do")
-- RbMethod Nothing
getKind :: Text -> Kind
getKind input = fromRight $ parseString line "<stdin>" $ unpack input
  where
    fromRight (Right x) = x
    fromRight (Left _) = error "kind can not be parsed"

-- |
--
-- >>> parseRakeRouteLine []
-- []
--
-- >>> parseRakeRouteLine (map pack ["pages GET /pages(.:format) {:action=>\"index\", :controller=>\"pages\"}"])
-- [RailsRoute {namedRoute = "pages", httpMethod = "GET", routeMap = "/pages(.:format)", controllerName = "pages", actionName = "index"}]
--
parseRakeRouteLine :: [Line] -> [RailsRoute]
parseRakeRouteLine input = rights $ map ((parseString route_line "<stdin>") . unpack) input

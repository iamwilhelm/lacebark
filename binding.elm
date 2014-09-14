module Binding where

import Dict
import Entity
import Glyph

-- Scope is the environment of variables that the compiler is able to see at that level.
type Scope = {
    variables: Dict.Dict String Float
  , toolbar: Glyph.Toolbar
  --, table: 
}

-- Binding is a stack of scopes that does look ups
type Binding = {
    stack: [Scope]
  , glyph: Glyph.Glyph
}

initialBinding : Glyph.Toolbar -> Binding
initialBinding toolbar =
  {
    stack = [{
      variables = Dict.empty
    , toolbar = toolbar
    }]
  , glyph = Glyph.selectedGlyph toolbar
  }


currentScope : Binding -> Scope
currentScope binding =
  head binding.stack

glyphsOf binding =
  (currentScope binding).toolbar.glyphs

selectedGlyphOf binding =
  Glyph.selectedGlyph (currentScope binding).toolbar

glyphOf binding =
  binding.glyph

varsOf binding =
  (currentScope binding).variables

setVar : Scope -> String -> Float -> Scope
setVar scope name value =
  { scope | variables <- Dict.insert name value scope.variables }

getVar scope name =
  Dict.getOrElse 0 name scope.variables


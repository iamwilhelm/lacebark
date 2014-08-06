module Toolbar where

import Entity
import Glyph

type Toolbar = {
    entity: Entity.Entity
  , selected: Float
  , glyphs: [Glyph.Glyph]
  }  

initialToolbar = {
    entity = {
      pos = (0, 0)
    , vel = (0, 0)
    , rot = 0
    , dim = (10, 10)
    , radius = 75
    , colr = blue
    }
  , selected = 0
  , glyphs = [
    --  Glyph.scratchGlyph
    --, Glyph.tentacleGlyph
      Glyph.rectangleGlyph
    --, Glyph.circGlyph
    --, Glyph.clubGlyph
    --, Glyph.heartGlyph
    --, Glyph.diamondGlyph
    ]
  }

selectedGlyph : Toolbar -> Glyph.Glyph
selectedGlyph toolbar =
  head toolbar.glyphs



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

draw { glyphs } =
  toForm
  <| flow down
  <| map (\glyph -> collage 50 50 [scale 0.15 <| Glyph.draw glyph]) glyphs

-- a hack to move and show the toolbar. toolbar should really just be a glyph that
-- contains other glyphs
transform windowDim toolbar =
  move (-(fst windowDim) / 2 + 50, 0) toolbar



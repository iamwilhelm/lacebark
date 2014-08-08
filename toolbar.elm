module Toolbar where

import Entity
import Glyph
import Dict

type Toolbar = {
    entity: Entity.Entity
  , selected: String
  , glyphs: Dict.Dict String Glyph.Glyph
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
  , selected = "rectangle"
  , glyphs = Dict.fromList [
    --  Glyph.scratchGlyph
    --, Glyph.tentacleGlyph
      ("circle", Glyph.circGlyph)
    , ("rectangle", Glyph.rectangleGlyph)
    --, Glyph.clubGlyph
    --, Glyph.heartGlyph
    --, Glyph.diamondGlyph
    ]
  }

selectedGlyph : Toolbar -> Glyph.Glyph
selectedGlyph toolbar =
  Dict.getOrFail toolbar.selected toolbar.glyphs

draw : Toolbar -> Form
draw ({ glyphs } as toolbar) =
  toForm <| flow down
         <| Dict.foldl (\key glyph cuml ->
              collage 50 50 [
                scale 0.15 <| Glyph.draw glyph
              , drawSelectedOutline key toolbar
              ] :: cuml
            ) [] glyphs

-- a hack to move and show the toolbar. toolbar should really just be a glyph that
-- contains other glyphs
transform windowDim toolbar =
  move (-(fst windowDim) / 2 + 50, 0) toolbar

asList toolbar =
  Dict.values toolbar.glyphs

drawSelectedOutline : String -> Toolbar -> Form
drawSelectedOutline key toolbar =
  if key == toolbar.selected then
    outlined (dotted black) <| square 48
  else
    filled black <| circle 0

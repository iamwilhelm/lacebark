module Glyph where

import Entity (..)

-- a glyph is a combination of shapes. glyph can be composed of many other
type Glyph = (Entity, Entity -> Int -> Form)

-- drawing functions without coordinate transforms

draw: Glyph -> Form
draw (entity, entityForm) =
  entityForm entity 10

drawAsCursor : Glyph -> Form
drawAsCursor (entity, entityForm) =
  move entity.pos <| scale 0.5 <| entityForm entity 10

drawToolbar { glyphTools } =
  toForm
  <| flow down
  <| map (\glyph -> collage 50 50 [scale 0.15 <| draw glyph]) glyphTools


-- a hack to move and show the toolbar. toolbar should really just be a glyph that
-- contains other glyphs
transformToolbar windowDim toolbar =
  move (-(fst windowDim) / 2 + 50, 0) toolbar

-- generic 'language' constructs

include : Glyph -> Form
include (entity, entityForm) =
  -- renderGlyph (entity entityForm)
  move entity.pos <| entityForm entity 10

rinclude : Glyph -> Int -> Form
rinclude (entity, entityForm) depth =
  case depth of
    0 ->
      group []
    _ ->
      group [ entityForm entity (depth - 1) ]



rectangle : Float -> Float -> Color -> Glyph
rectangle w h colr =
  let
    entity = { initialEntity |
                 colr <- colr
               , dim <- (w, h) }

    entityForm entity depth =
      group [
        filled entity.colr <| uncurry rect entity.dim
      ]
  in
    (entity, entityForm)


circ : Float -> Color -> Glyph
circ radius colr =
  let
    entity = { initialEntity |
        colr <- colr
      , radius <- radius }

    entityForm entity depth =
      group [
        filled entity.colr <| circle entity.radius
      ]
  in
    (entity, entityForm)


club : Color -> Glyph
club colr =
  let
    entity =
      { initialEntity |
          colr <- colr }

    entityForm glyph depth =
      group [
        move (0, 60)
          <| include (circ 40 entity.colr)
      , move (35, 0)
          <| include (circ 40 entity.colr)
      , move (-35, 0)
          <| include (circ 40 entity.colr)
      , move (0, -35)
          <| include (rectangle 20 80 glyph.colr)
      ]
  in
    (entity, entityForm)


heart : Color -> Glyph
heart colr =
  let
    entity = { initialEntity |
        colr <- colr }

    entityForm glyph depth =
      group [
        move (30, 30)
          <| filled red <| circle 50
      , move (-30, 30)
          <| filled red <| circle 50
      , rotate (degrees 45)
          <| move (0, 0)
            <| filled red
              <| rect 100 100
      ]
  in
    (entity, entityForm)


diamond : Color -> Glyph
diamond colr =
  let
    entity = { initialEntity |
        colr <- colr }

    entityForm glyph depth =
      group [ filled red <| ngon 4 80 ]
  in
    (entity, entityForm)

----- Hardcoded glyphs

arrowCursor =
  let
    entity = initialEntity

    entityForm : Entity -> Int -> Form
    entityForm glyph depth =
      group [
        filled black <| ngon 3 20
      , move (8.25, -15)
        <| rotate (degrees 30)
        <| filled black <| rect 10 25
      ]
  in
    (entity, entityForm)


openPawCursor =
  let
    entity = initialEntity

    entityForm : Entity -> Int -> Form
    entityForm glyph depth =
      group [
        include (circ 18 black)
      , rotate (degrees -25)
        <| group <| map (\ang ->
                      rotate (degrees ang)
                      <| group [ move (0, 23) <| include (circ 7.5 black) ]
                    ) [-45, 0, 45]
      , rotate (degrees 85) <| group [ move (0, 23) <| include (circ 7.5 black) ]
      ]
  in
    (entity, entityForm)


closedPawCursor =
  let
    entity = initialEntity

    entityForm : Entity -> Int -> Form
    entityForm glyph depth =
      group [
        include (circ 18 black)
      , rotate (degrees -25)
        <| group <| map (\ang ->
                      rotate (degrees ang)
                      <| group [ move (0, 17) <| include (circ 7.5 black) ]
                    ) [-45, 0, 45]
      , rotate (degrees 80) <| group [ move (0, 17) <| include (circ 7.5 black) ]
      ]
  in
    (entity, entityForm)


rectangleGlyph = rectangle 120 120 blue
circGlyph = circ 60 orange
clubGlyph = club charcoal
heartGlyph = heart red
diamondGlyph = diamond red

tentacleGlyph =
  let
    entity = { initialEntity |
      colr <- yellow }

    entityForm : Entity -> Int -> Form
    entityForm entity depth =
      group [
        include rectangleGlyph
      ,
        rotate (degrees 30)
        <| scale 0.7
        <| move (80, 80)
        <| rinclude tentacleGlyph depth
      ]
  in
    (entity, entityForm)

scratchGlyph =
  let
    entity = initialEntity
    entityForm entity depth =
      group [
        include rectangleGlyph
      , move (100, 100) <| include circGlyph
      , move (-150, 50) <| include clubGlyph
      ]
  in
    (entity, entityForm)



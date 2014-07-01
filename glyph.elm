module Glyph where

import Entity (..)
import G (..)

-- a glyph is a combination of shapes. glyph can be composed of many other

type Glyph = {
    entity: Entity
  , statements: [Statement]
  , history: [ [Statement] ]
  }

-- drawing functions without coordinate transforms

draw: Glyph -> Form
draw { entity, statements } =
  group <| map (\s -> renderTexel s) statements

drawAsCursor : Glyph -> Form
drawAsCursor ({ entity } as glyph) =
  move entity.pos <| scale 0.5 <| draw glyph

drawToolbar { glyphTools } =
  toForm
  <| flow down
  <| map (\glyph -> collage 50 50 [scale 0.15 <| draw glyph]) glyphTools


-- a hack to move and show the toolbar. toolbar should really just be a glyph that
-- contains other glyphs
transformToolbar windowDim toolbar =
  move (-(fst windowDim) / 2 + 50, 0) toolbar


-- defining various default glyphs


rectangle : Float -> Float -> Color -> Glyph
rectangle w h colr =
  let
    entity = { initialEntity |
        colr <- colr
      , dim <- (w, h)
      }
    statements = [ Draw NoTF (Rectangle (120, 120) blue) ]
    history = [ statements ]
  in
    { entity = entity, statements = statements, history = history }


circ : Float -> Color -> Glyph
circ radius colr =
  let
    entity = { initialEntity |
        colr <- colr
      , radius <- radius
      }
    statements = [ Draw (NoTF) (Circle 70 orange) ]
    history = [ statements ]
  in
    { entity = entity, statements = statements, history = history }


club : Color -> Glyph
club colr =
  let
    entity =
      { initialEntity |
          colr <- colr
      }
    statements = [
        Draw (Move (0, 60)) (Circle 40 charcoal)
      , Draw (Move (35, 0)) (Circle 40 charcoal)
      , Draw (Move (-35, 0))  (Circle 40 charcoal)
      , Draw (Move (0, -35)) (Rectangle (20, 80) charcoal)
    ]
  in
    { entity = entity, statements = statements, history = [ statements ] }


heart : Color -> Glyph
heart colr =
  let
    entity = { initialEntity |
        colr <- colr }

    statements = [
      Draw (Move (30, 30)) (Circle 50 red)
    , Draw (Move (-30, 30)) (Circle 50 red)
    , Draw (Rotate 45) (Rectangle (100, 100) red)
    ]
  in
    { entity = entity, statements = statements, history = [ statements ] }


diamond : Color -> Glyph
diamond colr =
  let
    entity = { initialEntity |
        colr <- colr }

    statements = [
      Draw (Rotate 45) (Rectangle (110, 110) red)
    ]
  in
    { entity = entity, statements = statements, history = [ statements ] }

----- Hardcoded glyphs

--arrowCursor =
--  let
--    entity = initialEntity
--
--    entityForm : Entity -> Int -> Form
--    entityForm glyph depth =
--      group [
--        filled black <| ngon 3 20
--      , move (8.25, -15)
--        <| rotate (degrees 30)
--        <| filled black <| rect 10 25
--      ]
--  in
--    (entity, entityForm)


openPawCursor =
  let
    entity = initialEntity
    statements = [
      Draw (Compose (Rotate 20) (Move (0, 23))) (Circle 7.5 black)
    , Draw (Compose (Rotate -70) (Move (0, 23))) (Circle 7.5 black)
    , Draw (Compose (Rotate -25) (Move (0, 23))) (Circle 7.5 black)
    , Draw (Compose (Rotate 85) (Move (0, 23))) (Circle 7.5 black)
    , Draw NoTF (Circle 18 black)
    ]
  in
    { entity = entity, statements = statements, history = [statements] }


closedPawCursor =
  let
    entity = initialEntity
    statements = [
      Draw (Compose (Rotate 20) (Move (0, 17))) (Circle 7.5 black)
    , Draw (Compose (Rotate -70) (Move (0, 17))) (Circle 7.5 black)
    , Draw (Compose (Rotate -25) (Move (0, 17))) (Circle 7.5 black)
    , Draw (Compose (Rotate 85) (Move (0, 17))) (Circle 7.5 black)
    , Draw NoTF (Circle 18 black)
    ]
  in
    { entity = entity, statements = statements, history = [statements] }


rectangleGlyph = rectangle 120 120 blue
circGlyph = circ 60 orange
clubGlyph = club charcoal
heartGlyph = heart red
diamondGlyph = diamond red

--tentacleGlyph =
--  let
--    entity = { initialEntity |
--      colr <- yellow }
--
--    entityForm : Entity -> Int -> Form
--    entityForm entity depth =
--      group [
--        include rectangleGlyph
--      ,
--        rotate (degrees 30)
--        <| scale 0.7
--        <| move (80, 80)
--        <| rinclude tentacleGlyph depth
--      ]
--  in
--    (entity, entityForm)

--scratchGlyph =
--  let
--    entity = initialEntity
--    entityForm entity depth =
--      group [
--        include rectangleGlyph
--      , move (100, 100) <| include circGlyph
--      --, move (-150, 50) <| include clubGlyph
--      ]
--  in
--    (entity, entityForm)



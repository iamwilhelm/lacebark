module Glyph where

import Transform2D
import Vec
import Entity

data Statement =
    NoOp
  | Block [Statement]
  | Move Term Statement
  | Rotate Term Statement
  | Scale Term Statement
  | Draw Contour

data Contour =
    Rectangle Term Color
  | Circle Term Color

data Term =
    Size Float Float
  | Radius Float
  | Pos Float Float
  | Rotation Float
  | Magnification Float Float
  | SpreadSheet Int Int
  | EntityOffset Float Float
  | EntityPos
  | EntityDim


compile : Glyph -> Statement -> Form
compile glyph statement =
  case statement of
    NoOp ->
      -- TODO don't know how to have an empty Glyph. use Maybe?
      filled black <| rect 1 1
    Block statements ->
      group <| map (\statement -> compile glyph statement) statements
    Move (Pos x y) statement ->
      group [move (x, y) <| compile glyph statement]
    Move EntityPos statement ->
      group [move glyph.entity.pos (compile glyph statement)]
    Move (EntityOffset x y) statement ->
      group [move (Vec.add glyph.entity.pos (x, y)) (compile glyph statement)]
    Rotate (Rotation r) statement ->
      rotate (degrees r) <| compile glyph statement
    Scale (Magnification x y) statement ->
      groupTransform
        (Transform2D.multiply (Transform2D.scaleX x) (Transform2D.scaleY y))
        [compile glyph statement]
    Draw contour ->
      compileContour glyph contour

compileContour : Glyph -> Contour -> Form
compileContour { entity } contour =
  case contour of
    Rectangle (Size w h) colr ->
      filled colr <| rect w h
    Rectangle (Radius r) colr ->
      filled colr <| rect (r * sqrt 2) (r * sqrt 2)
    Rectangle EntityDim colr ->
      filled colr <| uncurry rect entity.dim
    Circle (Radius r) colr ->
      filled colr <| circle r

-- a glyph is a combination of shapes. glyph can be composed of many other

type Glyph = {
    entity: Entity.Entity
  , statements: [Statement]
  , history: [ [Statement] ]
  }

-- drawing functions without coordinate transforms

draw: Glyph -> Form
draw ({ entity, statements } as glyph) =
  group <| map (\statement -> compile glyph statement) statements

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

setColr : Glyph -> Color -> Glyph
setColr ({ entity } as glyph) colr =
  { glyph | entity <-
      Entity.setColr glyph.entity colr
  }

setPos : Glyph -> Float -> Float -> Glyph
setPos ({ entity } as glyph) x y =
  { glyph | entity <-
    Entity.setPos glyph.entity x y
  }

-- defining various default glyphs


rectangle : Float -> Float -> Color -> Glyph
rectangle w h colr =
  let
    entity = {
        pos = (0, 0)
      , vel = (0, 0)
      , rot = 0
      , dim = (w, h)
      , radius = sqrt (w * w + h * h)
      , colr = colr
      }
    statements = [
      Move (Pos 0 50) (Block [
        Draw (Rectangle EntityDim entity.colr)
      , Draw (Circle (Radius 30) orange)
      , Move (Pos 60 0) (Block [
          Draw (Rectangle (Size 80 20) blue)
        , Draw (Rectangle (Size 20 80) red)
        ])
      ])
    ]
    history = [ statements ]
  in
    { entity = entity, statements = statements, history = history }


--circ : Float -> Color -> Glyph
--circ radius colr =
--  let
--    original = Entity.initialEntity
--    entity = { original |
--        colr <- colr
--      , radius <- radius
--      }
--    statements = [ Draw (NoTF) (Circle entity.radius entity.colr) ]
--    history = [ statements ]
--  in
--    { entity = entity, statements = statements, history = history }


--club : Color -> Glyph
--club colr =
--  let
--    original = Entity.initialEntity
--    entity =
--      { original |
--          colr <- colr
--        }
--    statements = [
--        Draw (Move (0, 60)) (Circle 40 charcoal)
--      , Draw (Move (35, 0)) (Circle 40 charcoal)
--      , Draw (Move (-35, 0))  (Circle 40 charcoal)
--      , Draw (Move (0, -35)) (Rectangle (20, 80) charcoal)
--    ]
--  in
--    { entity = entity, statements = statements, history = [ statements ] }
--
--
--heart : Color -> Glyph
--heart colr =
--  let
--    original = Entity.initialEntity
--    entity = { original |
--        colr <- colr }
--
--    statements = [
--      Draw (Move (30, 30)) (Circle 50 red)
--    , Draw (Move (-30, 30)) (Circle 50 red)
--    , Draw (Rotate 45) (Rectangle (100, 100) red)
--    ]
--  in
--    { entity = entity, statements = statements, history = [ statements ] }
--
--
--diamond : Color -> Glyph
--diamond colr =
--  let
--    original = Entity.initialEntity
--    entity = { original |
--        colr <- colr }
--
--    statements = [
--      Draw (Rotate 45) (Rectangle (110, 110) red)
--    ]
--  in
--    { entity = entity, statements = statements, history = [ statements ] }

----- Hardcoded glyphs

--arrowCursor =
--  let
--    entity = Entity.initialEntity
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


--openPawCursor =
--  let
--    entity = Entity.initialEntity
--    statements = [
--      Draw (Compose (Rotate 20) (Move (0, 23))) (Circle 7.5 black)
--    , Draw (Compose (Rotate -70) (Move (0, 23))) (Circle 7.5 black)
--    , Draw (Compose (Rotate -25) (Move (0, 23))) (Circle 7.5 black)
--    , Draw (Compose (Rotate 85) (Move (0, 23))) (Circle 7.5 black)
--    , Draw NoTF (Circle 18 black)
--    ]
--  in
--    { entity = entity, statements = statements, history = [statements] }
--
--
--closedPawCursor =
--  let
--    entity = Entity.initialEntity
--    statements = [
--      Draw (Compose (Rotate 20) (Move (0, 17))) (Circle 7.5 black)
--    , Draw (Compose (Rotate -70) (Move (0, 17))) (Circle 7.5 black)
--    , Draw (Compose (Rotate -25) (Move (0, 17))) (Circle 7.5 black)
--    , Draw (Compose (Rotate 85) (Move (0, 17))) (Circle 7.5 black)
--    , Draw NoTF (Circle 18 black)
--    ]
--  in
--    { entity = entity, statements = statements, history = [statements] }


rectangleGlyph = rectangle 120 120 purple
--circGlyph = circ 60 orange
--clubGlyph = club charcoal
--heartGlyph = heart red
--diamondGlyph = diamond red

--tentacleGlyph =
--  let
--    entity = { Entity.initialEntity |
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
--    entity = Entity.initialEntity
--    entityForm entity depth =
--      group [
--        include rectangleGlyph
--      , move (100, 100) <| include circGlyph
--      --, move (-150, 50) <| include clubGlyph
--      ]
--  in
--    (entity, entityForm)



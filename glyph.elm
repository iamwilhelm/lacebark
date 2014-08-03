module Glyph where

import Transform2D
import Vec
import Entity
import Dict

data Statement =
    NoOp
  | Block [Statement]
  | Move Term Statement
  | Rotate Term Statement
  | Scale Term Statement
  | Draw Contour
  | Map Statement [Float]

data Contour =
    Rectangle Term Color
  | Circle Term Color

-- Change Size, Pos, and Magnification to Tuple?
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
  | BindN Float Float

compile : Glyph -> Statement -> Form
compile glyph statement =
  case statement of
    NoOp ->
      -- TODO don't know how to have an empty Glyph. use Maybe?
      filled black <| rect 1 1
    Block statements ->
      group <| map (\statement -> compile glyph statement) statements
    Move term statement ->
      -- TODO maybe group rotate instead of move, as it occurs less to perserve order of
      -- operations?
      group [move (compileMoveTerm glyph term) <| compile glyph statement]
    Rotate term statement ->
      rotate (compileRotationTerm glyph term) <| compile glyph statement
    Scale (Magnification x y) statement ->
      groupTransform
        (Transform2D.multiply (Transform2D.scaleX x) (Transform2D.scaleY y))
        [compile glyph statement]
    Draw contour ->
      compileContour glyph contour
    Map statement list ->
      group <| map (\n -> compile (setVar glyph "n" n) statement) list


compileMoveTerm : Glyph -> Term -> (Float, Float)
compileMoveTerm glyph term =
  case term of
    Pos x y ->
      (x, y)
    EntityPos ->
      glyph.entity.pos
    EntityOffset x y ->
      Vec.add glyph.entity.pos (x, y)
    BindN x y ->
      (x, y + getVar glyph "n")
    --_ ->
    --  raise error

compileRotationTerm : Glyph -> Term -> Float
compileRotationTerm glyph term =
  case term of
    Rotation r ->
      degrees r

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
  , binding: Dict.Dict String Float
  }

setVar glyph name value =
  { glyph |
      binding <- Dict.insert name value glyph.binding
  }

getVar glyph name =
  Dict.getOrElse 0 name glyph.binding

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

setDim : Glyph -> Float -> Float -> Glyph
setDim ({ entity } as glyph) w h =
  { glyph | entity <-
    Entity.setDim glyph.entity w h
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
        Draw (Rectangle EntityDim entity.colr)
      , Draw (Circle (Radius 30) orange)
      , Map (Block [
          Move (BindN 60 -100) (Block [
            Draw (Rectangle (Size 40 20) blue)
          , Draw (Rectangle (Size 20 40) red)
          ])
        ]) [0, 80, 160, 240, 320]
    ]
    history = [ statements ]
    binding = Dict.empty
  in
    { entity = entity, statements = statements, history = history, binding = binding }


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


openPawCursor =
  let
    entity = {
        pos = (0, 0)
      , vel = (0, 0)
      , rot = 0
      , dim = (1, 1)
      , radius = 5
      , colr = black
      }
    statements = [
      Rotate (Rotation 20) (Move (Pos 0 23) (
        Draw (Circle (Radius 7.5) black)
      ))
    , Rotate (Rotation -70) (Move (Pos 0 23) (
        Draw (Circle (Radius 7.5) black)
      ))
    , Rotate (Rotation -25) (Move (Pos 0 23) (
        Draw (Circle (Radius 7.5) black)
      ))
    , Rotate (Rotation 85) (Move (Pos 0 23) (
        Draw (Circle (Radius 7.5) black)
      ))
    , Draw (Circle (Radius 18) black)
    ]
    binding = Dict.empty
  in
    { entity = entity, statements = statements
    , history = [statements], binding = binding }


closedPawCursor =
  let
    entity = Entity.initialEntity
    statements = [
      Rotate (Rotation 20) (Move (Pos 0 17) (
        Draw (Circle (Radius 7.5) black)
      ))
    , Rotate (Rotation -70) (Move (Pos 0 17) (
        Draw (Circle (Radius 7.5) black)
      ))
    , Rotate (Rotation -25) (Move (Pos 0 17) (
        Draw (Circle (Radius 7.5) black)
      ))
    , Rotate (Rotation 85) (Move (Pos 0 17) (
        Draw (Circle (Radius 7.5) black)
      ))
    , Draw (Circle (Radius 18) black)
    ]
    binding = Dict.empty
  in
    { entity = entity, statements = statements,
      history = [statements], binding = binding }


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



module Glyph where

import Dict
import Array
import Vec
import Entity
import BoundingBox
import L (..)

import Debug

-- a glyph is a combination of shapes. glyph can be composed of many other

type Glyph = {
    entity: Entity.Entity
  , statements: [Statement]
  , history: [ [Statement] ]
  , binding: Dict.Dict String Float
  }

--drawRubberband : Glyph -> Form
--drawRubberband glyph =
--  let
--    boundingbox = getBoundingBox glyph
--    size = fst boundingbox
--    offset = snd boundingbox
--  in
--    move offset
--    <| group [
--         (outlined (dashed black) <| uncurry rect <| size)
--       , move (-(fst size) / 2, (snd size) / 2) <| filled black <| circle 5
--       , move (0, (snd size) / 2) <| filled black <| circle 5
--       , move ((fst size) / 2, (snd size) / 2) <| filled black <| circle 5
--       , move (-(fst size) / 2, 0) <| filled black <| circle 5
--       , move ((fst size) / 2, 0) <| filled black <| circle 5
--       , move (-(fst size) / 2, -(snd size) / 2) <| filled black <| circle 5
--       , move (0, -(snd size) / 2) <| filled black <| circle 5
--       , move ((fst size) / 2, -(snd size) / 2) <| filled black <| circle 5
--       ]

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


--getBoundingBox : Glyph -> BoundingBox.RangedOffset
--getBoundingBox glyph =
--  boundsForStatement glyph (Block glyph.statements)
--
--
--boundsForStatement : Glyph -> Statement -> BoundingBox.RangedOffset
--boundsForStatement glyph statement =
--  case statement of
--    NoOp ->
--      ((0, 0), (0, 0))
--    Block statements ->
--      foldl (\t x -> BoundingBox.merge t x)
--        (boundsForStatement glyph <| head statements)
--        (map (\x -> boundsForStatement glyph x) <| tail statements)
--    Move term statement ->
--      let
--        bbox = boundsForStatement glyph statement
--        size = fst bbox
--        offset = snd bbox
--      in
--        (size, Vec.add offset <| compileTupTerm glyph term)
--    Scale term statement ->
--      let
--        bbox = boundsForStatement glyph statement
--        size = fst bbox
--        offset = snd bbox
--      in
--        (Vec.scale size <| compileTupTerm glyph term,
--         Vec.scale offset <| compileTupTerm glyph term)
--    Rotate term statement ->
--      boundsForStatement glyph statement
--
--    Draw contour ->
--      boundsForContour glyph contour
--
--boundsForContour : Glyph -> Contour -> (Vec.Vec, Vec.Vec)
--boundsForContour ({ entity } as glyph) contour =
--  case contour of
--    Rectangle (Tup w h) colr ->
--      ((compileNumTerm glyph w, compileNumTerm glyph h), (0, 0))
--    Rectangle EntityDim colr ->
--      (entity.dim, (0, 0))
--    Circle r colr ->
--      let
--        radius = 2 * compileNumTerm glyph r
--      in
--        ((radius, radius), (0, 0))
--    Triangle r colr ->
--      let
--        radius = 2 * compileNumTerm glyph r
--      in
--        ((radius, radius), (0, 0))

----- Toolbar -----

type Toolbar = {
    entity: Entity.Entity
  , selected: String
  , glyphs: Dict.Dict String Glyph
  }

initialToolbar windowDim = {
    entity = {
      pos = (0, (snd windowDim / 2 - 50))
    , vel = (0, 0)
    , rot = 0
    , dim = (7 * 50, 50)
    , radius = 0
    , colr = blue
    }
  , selected = "scratch"
  , glyphs = Dict.fromList [
      ("scratch", scratchGlyph)
    , ("circle", circGlyph)
    , ("rectangle", rectangleGlyph)
    , ("redcross", redcrossGlyph)
    , ("arrow", arrowCursor)
    , ("openPaw", openPawCursor)
    , ("closedPaw", closedPawCursor)
    --, Glyph.tentacleGlyph
    --, Glyph.clubGlyph
    --, Glyph.heartGlyph
    --, Glyph.diamondGlyph
    ]
  }

toolbarTop : Toolbar -> Float
toolbarTop toolbar =
  (snd toolbar.entity.pos) + (snd toolbar.entity.dim) / 2

toolbarBottom : Toolbar -> Float
toolbarBottom toolbar =
  (snd toolbar.entity.pos) - (snd toolbar.entity.dim) / 2

numOfGlyphs : Toolbar -> Int
numOfGlyphs toolbar =
  length <| Dict.toList toolbar.glyphs

selectGlyph : Toolbar -> Float -> Float -> String
selectGlyph toolbar x y =
  let
    names = Dict.keys toolbar.glyphs
    xcoord = x + (fst toolbar.entity.dim) / 2
    xindex = floor (xcoord /
                    ((fst toolbar.entity.dim) / (toFloat <| numOfGlyphs toolbar)))
  in
    if ((xindex >= 0) && (xindex < (length names)))
        && ((y > toolbarBottom toolbar) && (y < toolbarTop toolbar)) then
      Array.getOrElse "scratch" xindex (Array.fromList names)
    else
      toolbar.selected

selectedGlyph : Toolbar -> Glyph
selectedGlyph toolbar =
  Dict.getOrFail toolbar.selected toolbar.glyphs

asList toolbar =
  Dict.values toolbar.glyphs

drawToolbarSelection : String -> Toolbar -> Form
drawToolbarSelection key toolbar =
  if key == toolbar.selected then
    outlined (dotted black) <| square 48
  else
    filled black <| circle 0

getGlyph : Toolbar -> String -> Glyph
getGlyph toolbar key =
  Dict.getOrFail key toolbar.glyphs

----- BNF for language -----


-- defining various default glyphs

scratchGlyph =
  let
    entity = {
      pos = (0, 0)
    , vel = (0, 0)
    , rot = 0
    , dim = (100, 100)
    , radius = 75
    , colr = blue
    }
    selected = Nothing
    statements = [
        Rotate (Degrees 60) (
          Move (Tup (F -100) (F -20)) (
            Draw (Rectangle EntityDim entity.colr)
          )
        )
      , Draw (Circle (F 50) orange)
      , Scale (Tup (F 5) (F 3)) (
          Move (Tup (F 50) (F -30)) (
            Draw (Rectangle (Tup (F 20) (F 100)) yellow)
          )
        )
      --  Move (Tup (F 0) (F 100)) (
      --    Scale (Tup (F 1) (F 2)) (
      --      Draw (Rectangle (Tup (F 150) (F 20)) red)
      --    )
      --  )
      -- , Map (Proc M [
      --     Move (Tup (Sub (Mul (F 100) M) (F 300)) (F 60)) (Include "redcross")
      --   ]) [1..5]
      -- , Move (Tup (F -100) (F 0)) (Include "circle")
      ]
    history = [statements]
    binding = Dict.empty
  in
    { entity = entity, statements = statements,
      history = history, binding = binding }


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
    statements = [ Draw (Rectangle (Tup (F w) (F h)) colr) ]
    history = [ statements ]
    binding = Dict.empty
  in
    { entity = entity, statements = statements,
      history = history, binding = binding }


circ : Float -> Color -> Glyph
circ radius colr =
  let
    original = Entity.initialEntity
    entity = { original |
        colr <- colr
      , radius <- radius
      }
    statements = [ Draw (Circle (F radius) colr) ]
    history = [ statements ]
    binding = Dict.empty
  in
    { entity = entity, statements = statements,
      history = history, binding = binding }


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

arrowCursor =
  let
    entity = Entity.initialEntity

    statements = [
        Draw (Triangle (F 20) black)
      , Move (Tup (F 8.25) (F -15)) (
          Rotate (Degrees 30) (
            Draw (Rectangle (Tup (F 10) (F 25)) black)
          )
        )
      ]
    history = [statements]
    binding = Dict.empty
  in
    { entity = entity, statements = statements, history = history, binding = binding }

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
      Rotate (Degrees 20) (Move (Tup (F 0) (F 23)) (
        Draw (Circle (F 7.5) black)
      ))
    , Rotate (Degrees -70) (Move (Tup (F 0) (F 23)) (
        Draw (Circle (F 7.5) black)
      ))
    , Rotate (Degrees -25) (Move (Tup (F 0) (F 23)) (
        Draw (Circle (F 7.5) black)
      ))
    , Rotate (Degrees 85) (Move (Tup (F 0) (F 23)) (
        Draw (Circle (F 7.5) black)
      ))
    , Draw (Circle (F 18) black)
    ]
    binding = Dict.empty
  in
    { entity = entity, statements = statements
    , history = [statements], binding = binding }


closedPawCursor =
  let
    entity = Entity.initialEntity
    statements = [
      Rotate (Degrees 20) (Move (Tup (F 0) (F 17)) (
        Draw (Circle (F 7.5) black)
      ))
    , Rotate (Degrees -70) (Move (Tup (F 0) (F 17)) (
        Draw (Circle (F 7.5) black)
      ))
    , Rotate (Degrees -25) (Move (Tup (F 0) (F 17)) (
        Draw (Circle (F 7.5) black)
      ))
    , Rotate (Degrees 85) (Move (Tup (F 0) (F 17)) (
        Draw (Circle (F 7.5) black)
      ))
    , Draw (Circle (F 18) black)
    ]
    binding = Dict.empty
  in
    { entity = entity, statements = statements,
      history = [statements], binding = binding }

rectangleGlyph = rectangle 140 140 purple
circGlyph = circ 80 green

redcrossGlyph =
  let
    entity = {
      pos = (0, 0)
    , vel = (0, 0)
    , rot = 0
    , dim = (80, 80)
    , radius = 75
    , colr = blue
    }
    statements = [
        Draw (Rectangle (Tup (F 80) (F 40)) red)
      , Draw (Rectangle (Tup (F 40) (F 80)) red)
      ]
    history = [statements]
    binding = Dict.empty
  in
    { entity = entity, statements = statements, history = history, binding = binding }

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



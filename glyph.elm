module Glyph where

import Dict
import Transform2D
import Vec
import Entity

-- a glyph is a combination of shapes. glyph can be composed of many other

type Glyph = {
    entity: Entity.Entity
  , statements: [Statement]
  , history: [ [Statement] ]
  , binding: Dict.Dict String Float
  }

setVar glyph name value =
  { glyph | binding <- Dict.insert name value glyph.binding }

getVar glyph name =
  Dict.getOrElse 0 name glyph.binding

-- drawing functions without coordinate transforms

draw: Toolbar -> Glyph -> Form
draw toolbar ({ entity, statements } as glyph) =
  group <| map (\statement -> compile toolbar glyph statement) statements

drawAsCursor : Toolbar -> Glyph -> Form
drawAsCursor toolbar ({ entity } as glyph) =
  move entity.pos <| scale 0.5 <| draw toolbar glyph

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

----- Toolbar -----

type Toolbar = {
    entity: Entity.Entity
  , selected: String
  , glyphs: Dict.Dict String Glyph
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
  , selected = "scratch"
  , glyphs = Dict.fromList [
      ("scratch", scratchGlyph)
    , ("circle", circGlyph)
    , ("rectangle", rectangleGlyph)
    , ("redcross", redcrossGlyph)
    --, Glyph.tentacleGlyph
    --, Glyph.clubGlyph
    --, Glyph.heartGlyph
    --, Glyph.diamondGlyph
    ]
  }

selectedGlyph : Toolbar -> Glyph
selectedGlyph toolbar =
  Dict.getOrFail toolbar.selected toolbar.glyphs

drawToolbar : Toolbar -> Form
drawToolbar ({ glyphs } as toolbar) =
  toForm <| flow down
         <| Dict.foldl (\key glyph cuml ->
              collage 50 50 [
                scale 0.15 <| draw toolbar glyph
              , drawSelectedOutline key toolbar
              ] :: cuml
            ) [] glyphs

-- a hack to move and show the toolbar. toolbar should really just be a glyph that
-- contains other glyphs
transformToolbar windowDim toolbar =
  move (-(fst windowDim) / 2 + 50, 0) toolbar

asList toolbar =
  Dict.values toolbar.glyphs

drawSelectedOutline : String -> Toolbar -> Form
drawSelectedOutline key toolbar =
  if key == toolbar.selected then
    outlined (dotted black) <| square 48
  else
    filled black <| circle 0

getGlyph : Toolbar -> String -> Glyph
getGlyph toolbar key =
  Dict.getOrFail key toolbar.glyphs

----- BNF for language -----

data Statement =
    NoOp
  | Block [Statement]
  | Proc Term [Statement]
  | Move Term Statement
  | Rotate Term Statement
  | Scale Term Statement
  | Draw Contour
  | Map Statement [Float]
  | Include String

data Contour =
    Rectangle Term Color
  | Circle Term Color
  | Triangle Term Color

-- Change Size, Pos, and Magnification to Tuple?
data Term =
    Tup Term Term
  | F Float
  | Degrees Float
  | Ref Int Int
  | EntityOffset Float Float
  | EntityPos
  | EntityDim
  | Add Term Term
  | Sub Term Term
  | Mul Term Term
  | Div Term Term
  | N
  | M


compile : Toolbar -> Glyph -> Statement -> Form
compile toolbar glyph statement =
  case statement of
    NoOp ->
      -- TODO don't know how to have an empty Glyph. use Maybe?
      filled black <| circle 0
    Block statements ->
      group <| map (\statement -> compile toolbar glyph statement) statements
    Move term statement ->
      group [move (compileTupTerm glyph term) <| compile toolbar glyph statement]
    Rotate term statement ->
      rotate (compileNumTerm glyph term) <| compile toolbar glyph statement
    Scale (Tup x y) statement ->
      groupTransform
        (Transform2D.multiply
          (Transform2D.scaleX (compileNumTerm glyph x))
          (Transform2D.scaleY (compileNumTerm glyph y))
        )
        [compile toolbar glyph statement]
    Draw contour ->
      compileContour glyph contour
    Map (Proc itername statements) list ->
      let
        iterkey = compileVarName glyph itername
        setKey = setVar glyph iterkey
      in
        group <| concatMap (\n ->
          map (\statement -> compile toolbar (setKey n) statement) statements
        ) list
    Include childGlyphName ->
      draw toolbar (Dict.getOrFail childGlyphName toolbar.glyphs)

compileVarName glyph varname =
  case varname of
    N -> "n"
    M -> "m"

compileTupTerm : Glyph -> Term -> (Float, Float)
compileTupTerm glyph term =
  case term of
    EntityPos ->
      glyph.entity.pos
    EntityOffset x y ->
      Vec.add glyph.entity.pos (x, y)
    Tup x y ->
      (compileNumTerm glyph x, compileNumTerm glyph y)

compileNumTerm : Glyph -> Term -> Float
compileNumTerm glyph term =
  case term of
    Degrees r ->
      degrees r
    F r ->
      r
    N ->
      getVar glyph "n"
    M ->
      getVar glyph "m"
    Add term1 term2 ->
      compileNumTerm glyph term1 + compileNumTerm glyph term2
    Sub term1 term2 ->
      compileNumTerm glyph term1 - compileNumTerm glyph term2
    Mul term1 term2 ->
      compileNumTerm glyph term1 * compileNumTerm glyph term2
    Div term1 term2 ->
      compileNumTerm glyph term1 / compileNumTerm glyph term2

compileContour : Glyph -> Contour -> Form
compileContour ({ entity } as glyph) contour =
  case contour of
    Rectangle (Tup w h) colr ->
      filled colr <| rect (compileNumTerm glyph w) (compileNumTerm glyph h)
    Rectangle (F r) colr ->
      filled colr <| rect (r * sqrt 2) (r * sqrt 2)
    Rectangle EntityDim colr ->
      filled colr <| uncurry rect entity.dim
    Circle (F r) colr ->
      filled colr <| circle r
    Triangle (F r) colr ->
      filled colr <| ngon 3 r

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
    statements = [ Draw (Rectangle (Tup (F 100) (F 100)) red) ]
    history = [ statements ]
    binding = Dict.empty
  in
    { entity = entity, statements = statements, history = history, binding = binding }


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
    { entity = entity, statements = statements, history = history, binding = binding }


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


scratchGlyph =
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
        Rotate (Degrees 60) (
          Move (Tup (F 200) (F 0)) (
            Draw (Rectangle EntityDim entity.colr))
          )
      , Draw (Circle (F 30) orange)
      , Map (Proc M [
          Move (Tup (Sub (Mul (F 100) M) (F 300)) (F 60)) (Include "redcross")
        ]) [1..5]
      , Move (Tup (F -100) (F 0)) (Include "circle")
      ]
    history = [statements]
    binding = Dict.empty
  in
    { entity = entity, statements = statements, history = history, binding = binding }


rectangleGlyph = rectangle 120 120 purple
circGlyph = circ 60 green

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



import Window
import Mouse
import Keyboard
import Transform2D

type Vec = (Float, Float)

vecAdd : Vec -> Vec -> Vec
vecAdd (ax, ay) (bx, by) = (ax + bx, ay + by)

vecMulS : Vec -> Float -> Vec
vecMulS (x, y) a = (x * a, y * a)


center : (Int, Int) -> (Int, Int)
center (w, h) = (div w 2 , div h 2)

relativeMouse : (Int, Int) -> (Int, Int) -> (Int, Int)
relativeMouse (ox, oy) (x, y) = (x - ox, -(y - oy))

-- a hack to correct for width of toolbar
toolbarAdjust: (Int, Int) -> (Int, Int)
toolbarAdjust (x, y) = (x + 80, y)


-- a glyph is a combination of shapes. glyph can be composed of many other
-- shapes or itself 
type Entity = { pos: Vec, vel: Vec, rad: Float, colr: Color, dim: Vec, radius: Float }
type Glyph = (Entity, Entity -> Int -> Form)

initialEntity = { pos = (0, 0)
                , vel = (0, 0)
                , rad = 0

                , colr = blue
                , dim = (10, 10)
                , radius = 75
                }



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

rectangleGlyph = rectangle 120 120 blue


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

circleGlyph = circ 60 red


arrowGlyph =
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


openPawGlyph =
  let
    entity = initialEntity

    entityForm : Entity -> Int -> Form
    entityForm glyph depth =
      group [
        include (circ 18 black)
      , rotate (degrees -25)
        <| group <| map (\ang ->
                      rotate (degrees ang) <| group [ move (0, 23) <| include (circ 7.5 black) ]
                    ) [-45, 0, 45]
      , rotate (degrees 85) <| group [ move (0, 23) <| include (circ 7.5 black) ]
      ]
  in
    (entity, entityForm)

closedPawGlyph =
  let
    entity = initialEntity

    entityForm : Entity -> Int -> Form
    entityForm glyph depth =
      group [
        include (circ 18 black)
      , rotate (degrees -25)
        <| group <| map (\ang ->
                      rotate (degrees ang) <| group [ move (0, 17) <| include (circ 7.5 black) ]
                    ) [-45, 0, 45]
      , rotate (degrees 80) <| group [ move (0, 17) <| include (circ 7.5 black) ]
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

clubGlyph = club charcoal



--heartGlyph = { initialGlyph |
--    formGroup <- group [
--                   move (30, 30)
--                     <| filled red <| circle 50
--
--                 , move (-30, 30)
--                     <| filled red <| circle 50
--
--                 , rotate (degrees 45)
--                     <| move (0, 0)
--                       <| filled red
--                         <| rect 100 100
--                 ] }
--
--diamondGlyph = { initialGlyph |
--    formGroup <- group [ filled red <| ngon 4 80 ] }


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
      , move (100, 100) <| include circleGlyph
      , move (-150, 50) <| include clubGlyph
      ]
  in
    (entity, entityForm)

type Camera = Entity
initialCamera = initialEntity

throughCamera camera forms =
  let
    transform = Transform2D.translation (fst camera.pos) (snd camera.pos)
  in
    groupTransform transform forms 

type Scene = { camera: Camera, glyphTools: [Glyph], cursor: Glyph }
initialScene = {
    camera = initialCamera
  , glyphTools = [ scratchGlyph, tentacleGlyph, rectangleGlyph, circleGlyph, clubGlyph ]
  , cursor = openPawGlyph
  }

updateGlyph : AppInput -> Glyph -> Glyph
updateGlyph (dt, _, _, _) (entity, entityForm) =
  ({ entity |
     pos <- vecAdd entity.pos <| vecMulS entity.vel dt
   },
   entityForm)

updateGlyphTools : AppInput -> [Glyph] -> [Glyph]
updateGlyphTools appInput glyphTools =
  map (\glyph -> updateGlyph appInput glyph) glyphTools

updateCursor : AppInput -> Glyph -> Glyph
updateCursor (_, (mouseX, mouseY), mouseDown, _) _ =
  let (entity, entityForm) =
    if mouseDown == True then
      closedPawGlyph
    else
      openPawGlyph
  in
    ({ entity | pos <- (toFloat mouseX, toFloat mouseY) }, entityForm)

updateScene : AppInput -> Scene -> Scene
updateScene appInput scene =
  { scene | glyphTools <- updateGlyphTools appInput scene.glyphTools
          , cursor <- updateCursor appInput scene.cursor }


renderGlyph : Glyph -> Form
renderGlyph (entity, entityForm) =
  move entity.pos <| entityForm entity 10

renderCursor : Glyph -> Form
renderCursor (entity, entityForm) =
  move entity.pos <| scale 0.5 <| entityForm entity 10

renderScene scene =
  [ throughCamera scene.camera [renderGlyph (head scene.glyphTools)]
  , renderCursor scene.cursor
  ]

renderToolbar { glyphTools } =
  flow down
  <| map (\glyph -> collage 80 80 [scale 0.2 <| renderGlyph glyph ]) glyphTools

render : (Int, Int) -> Scene -> Element
render (w, h) scene =
  color lightGray
    <| container w h middle
    <| flow right [
         renderToolbar scene
       , color gray <| collage 1024 600 <| renderScene scene
       ]

type AppInput = (Time, (Int, Int), Bool, { x: Int, y: Int })

clockInput = lift inSeconds (fps 30)
mouseInput = sampleOn clockInput
               (lift2 relativeMouse
                      (center <~ (toolbarAdjust <~ Window.dimensions))
                      Mouse.position)
keyInput = Keyboard.wasd
input = (,,,) <~ clockInput ~ mouseInput ~ Mouse.isDown ~ keyInput

main = render <~ Window.dimensions ~ foldp updateScene initialScene input


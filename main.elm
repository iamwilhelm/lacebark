import Window
import Mouse
import Keyboard
import Transform2D

import Vec
import Entity (..)
import Glyph

----- Graphics pipeline
-- Frame coords
-- Glyph coords
-- World coords
-- Camera coords
-- Viewport coords
-- Window coords

center : (Int, Int) -> (Int, Int)
center (w, h) = (div w 2 , div h 2)

relativeMouse : (Int, Int) -> (Int, Int) -> (Int, Int)
relativeMouse (ox, oy) (x, y) = (x - ox, -(y - oy))

type Camera = Entity
initialCamera = initialEntity

type Axes = Entity
initialAxes = initialEntity

-- TODO Scene and tools overlay and cursor overlay should all be inserted in
-- different parts of the graphics transformation pipeline
type Scene = {
    camera: Camera
  , glyphTools: [Glyph.Glyph]
  , cursor: Glyph.Glyph
  , axes: Axes
  }

initialScene = {
    camera = initialCamera
  , glyphTools = [
      Glyph.scratchGlyph
    , Glyph.tentacleGlyph
    , Glyph.rectangleGlyph
    , Glyph.circGlyph
    , Glyph.clubGlyph
    , Glyph.heartGlyph
    , Glyph.diamondGlyph
    ]
  , cursor = Glyph.openPawCursor
  , axes = initialAxes
  }

updateGlyph : AppInput -> Glyph.Glyph -> Glyph.Glyph
updateGlyph (dt, _, _, _) (entity, entityForm) =
  ({ entity |
     pos <- Vec.add entity.pos <| Vec.mulS entity.vel dt
   },
   entityForm)

updateGlyphTools : AppInput -> [Glyph.Glyph] -> [Glyph.Glyph]
updateGlyphTools appInput glyphTools =
  map (\glyph -> updateGlyph appInput glyph) glyphTools

updateCursor : AppInput -> Glyph.Glyph -> Glyph.Glyph
updateCursor (_, (mouseX, mouseY), mouseDown, _) _ =
  let (entity, entityForm) =
    if mouseDown == True then
      Glyph.closedPawCursor
    else
      Glyph.openPawCursor
  in
    ({ entity | pos <- (toFloat mouseX, toFloat mouseY) }, entityForm)

updateCamera : AppInput -> Camera -> Camera
updateCamera (dt, _, _, keyDir) camera =
  let
    new_vec = Vec.mulS (toFloat keyDir.x, toFloat keyDir.y) -150
    new_pos = Vec.add camera.pos <| Vec.mulS camera.vel dt
  in
    { camera |
        pos <- new_pos
      , vel <- new_vec
    }

updateScene : AppInput -> Scene -> Scene
updateScene appInput scene =
  { scene | camera <- updateCamera appInput scene.camera
          , glyphTools <- updateGlyphTools appInput scene.glyphTools
          , cursor <- updateCursor appInput scene.cursor }


-- graphics transformation pipeline

data AxesDirection = XAxis | YAxis

drawAxesTicks : Axes -> AxesDirection -> Form
drawAxesTicks axes direction =
  group <| map (\x ->
      case direction of
        XAxis ->
          traced (solid black) (segment (x, 10) (x, -10))
        YAxis ->
          traced (solid black) (segment (10, x) (-10, x))
    )
    [-500, -400, -300, -200, -100, 100, 200, 300, 400, 500]

drawAxes : Axes -> Form
drawAxes axes =
  group [
    traced (solid black) (segment (0, -5000) (0, 5000))
  , drawAxesTicks axes XAxis
  , traced (solid black) (segment (5000, 0) (-5000, 0))
  , drawAxesTicks axes YAxis
  ]

glyph2World : Glyph.Glyph -> Transform2D.Transform2D
glyph2World (entity, entityForm) =
  Transform2D.multiply
    (uncurry Transform2D.translation entity.pos)
    (Transform2D.rotation (degrees entity.rot))

world2Camera : Camera -> Transform2D.Transform2D
world2Camera camera =
  Transform2D.multiply
    (uncurry Transform2D.translation camera.pos)
    (Transform2D.rotation (degrees camera.rot))

camera2Viewport : Transform2D.Transform2D
camera2Viewport =
  Transform2D.identity


renderScene : Scene -> Form
renderScene scene =
  let
    rootGlyph = head scene.glyphTools
  in
    groupTransform
      (Transform2D.multiply camera2Viewport
       <| Transform2D.multiply (world2Camera scene.camera)
       <| (glyph2World rootGlyph))
      [ drawAxes scene.axes
      , Glyph.draw rootGlyph
      ]

renderCursor : Glyph.Glyph -> Form
renderCursor (entity, entityForm) =
  move entity.pos <| scale 0.5 <| entityForm entity 10

renderToolbar { glyphTools } =
  flow down
  <| map (\glyph -> collage 50 50 [scale 0.15 <| Glyph.draw glyph ]) glyphTools


windowDim = (1024, 600)

render : (Int, Int) -> Scene -> Element
render (w, h) scene =
  color lightGray
    <| container w h middle
    <| flow right [
         color gray <| uncurry collage windowDim [
           renderScene scene
         , move (-(fst windowDim) / 2 + 50, 0) (toForm <| renderToolbar scene)
         , renderCursor scene.cursor
         ]
       ]

----- All signals and input into the program

type AppInput = (Time, (Int, Int), Bool, { x: Int, y: Int })

clockInput = lift inSeconds (fps 30)
mouseInput = sampleOn clockInput
               (lift2 relativeMouse
                      (center <~ Window.dimensions)
                      Mouse.position)
keyInput = Keyboard.wasd
input = (,,,) <~ clockInput ~ mouseInput ~ Mouse.isDown ~ keyInput

main = render <~ Window.dimensions ~ foldp updateScene initialScene input


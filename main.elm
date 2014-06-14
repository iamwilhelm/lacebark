import Window
import Mouse
import Keyboard
import Transform2D

import Vec
import Entity (..)
import Glyph
import Axes

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

-- TODO Scene and tools overlay and cursor overlay should all be inserted in
-- different parts of the graphics transformation pipeline
type Scene = {
    camera: Camera
  , glyphTools: [Glyph.Glyph]
  , cursor: Glyph.Glyph
  , axes: Axes.Axes
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
  , axes = Axes.initialAxes
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


glyph2WorldMat : Glyph.Glyph -> Transform2D.Transform2D
glyph2WorldMat (entity, entityForm) =
  Transform2D.multiply
    (uncurry Transform2D.translation entity.pos)
    (Transform2D.rotation (degrees entity.rot))

-- [Forms in this frame already] -> [Forms that need to be transformed to this frame] -> [Resulting Forms in this frame]
renderInWorldFrame : Glyph.Glyph -> [Form] -> [Form] -> [Form]
renderInWorldFrame glyph inFrameForms childFrameForms =
  groupTransform (glyph2WorldMat glyph) childFrameForms :: inFrameForms

world2CameraMat : Camera -> Transform2D.Transform2D
world2CameraMat camera =
  Transform2D.multiply
    (uncurry Transform2D.translation camera.pos)
    (Transform2D.rotation (degrees camera.rot))

renderInCameraFrame : Camera -> [Form] -> [Form] -> [Form]
renderInCameraFrame camera inFrameForms childFrameForms =
  -- clip all objects outside the camera
  groupTransform (world2CameraMat camera) childFrameForms :: inFrameForms

camera2ViewportMat : Transform2D.Transform2D
camera2ViewportMat =
  Transform2D.identity

renderInViewportFrame : [Form] -> [Form] -> [Form]
renderInViewportFrame inFrameForms childFrameForms =
  -- clip all objects outside the viewport
  groupTransform camera2ViewportMat childFrameForms :: inFrameForms

-- convert all draw() methods to toForm methods in namespace
renderScene : Scene -> [Form]
renderScene scene =
  let
    rootGlyph = head scene.glyphTools
  in
    renderInViewportFrame []
    <| renderInCameraFrame scene.camera [Glyph.drawAsCursor scene.cursor]
    <| renderInWorldFrame rootGlyph []
    <| [Axes.draw scene.axes, Glyph.draw rootGlyph]

    --, move (-(fst windowDim) / 2 + 50, 0) (toForm <| renderToolbar scene)

render : (Int, Int) -> Scene -> Element
render (w, h) scene =
  color lightGray
    <| container w h middle
    <| color gray
    <| uncurry collage windowDim
    <| renderScene scene

----- All signals and input into the program

type AppInput = (Time, (Int, Int), Bool, { x: Int, y: Int })

clockInput = lift inSeconds (fps 30)
mouseInput = sampleOn clockInput
               (lift2 relativeMouse
                      (center <~ Window.dimensions)
                      Mouse.position)
keyInput = Keyboard.wasd
input = (,,,) <~ clockInput ~ mouseInput ~ Mouse.isDown ~ keyInput

windowDim = (1024, 600)
main = render <~ Window.dimensions ~ foldp updateScene initialScene input


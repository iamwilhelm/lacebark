import Window
import Mouse
import Keyboard

import Vec
import Entity (..)
import Glyph
import Axes
import Camera
import Gpipeline

center : (Int, Int) -> (Int, Int)
center (w, h) = (div w 2 , div h 2)

relativeMouse : (Int, Int) -> (Int, Int) -> (Int, Int)
relativeMouse (ox, oy) (x, y) = (x - ox, -(y - oy))

-- TODO Scene and tools overlay and cursor overlay should all be inserted in
-- different parts of the graphics transformation pipeline

type Scene = {
    camera: Camera.Camera
  , glyphTools: [Glyph.Glyph]
  , cursor: Glyph.Glyph
  , axes: Axes.Axes
  }

initialCamera = initialEntity

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

addGlyph : AppInput -> Glyph.Glyph -> Glyph.Glyph
addGlyph (dt, mousePos, mouseDown, _) (entity, prevEntityForm) =
  let
    newEntityForm entity depth =
      group [
        prevEntityForm entity depth
      , move (toFloat <| fst mousePos, toFloat <| snd mousePos)
        <| filled green <| ngon 5 20
      ]
  in
    (entity, newEntityForm)

updateGlyphTools : AppInput -> [Glyph.Glyph] -> [Glyph.Glyph]
updateGlyphTools (dt, mousePos, mouseDown, keyDir) glyphTools =
  case mouseDown of
    True ->
      let
        headSet = take 0 glyphTools
        tailSet = drop 0 glyphTools
      in
        headSet ++ (
            addGlyph (dt, mousePos, mouseDown, keyDir) <| head tailSet
          ) :: tail tailSet
    False ->
      map (\glyph ->
        updateGlyph (dt, mousePos, mouseDown, keyDir) glyph
      ) glyphTools


updateCursor : AppInput -> Glyph.Glyph -> Glyph.Glyph
updateCursor (_, (mouseX, mouseY), mouseDown, _) _ =
  let (entity, entityForm) =
    if mouseDown == True then
      Glyph.closedPawCursor
    else
      Glyph.openPawCursor
  in
    ({ entity | pos <- (toFloat mouseX, toFloat mouseY) }, entityForm)

updateCamera : AppInput -> Camera.Camera -> Camera.Camera
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
  -- Gpipeline.updateInWorldFrame []
  -- <| Gpipeline.updateInWorldFrame []
  -- <| Gpipeline.updateInCameraFrame []
  -- <| Gpipeline.updateInViewportFrame []
  -- <| Gpipeline.updateInWindowFrame windowDim [
  -- ]

  let
    updatedCamera = updateCamera appInput scene.camera
    updatedGlyphTools = updateGlyphTools appInput scene.glyphTools
    updatedCursor = updateCursor appInput scene.cursor
  in
    { scene | camera <- updatedCamera
            , glyphTools <- updatedGlyphTools
            , cursor <- updatedCursor }

-- TODO convert all draw() methods to toForm methods in namespace
renderScene : Scene -> [Form]
renderScene scene =
  Gpipeline.renderInViewportFrame [
    Glyph.drawAsCursor scene.cursor
  ]
  <| Gpipeline.renderInCameraFrame scene.camera [
    Glyph.transformToolbar windowDim <| Glyph.drawToolbar scene
  ]
  <| Gpipeline.renderInWorldFrame (head scene.glyphTools) [
  ]
  <| [
    Axes.draw scene.axes
  , Glyph.draw <| head scene.glyphTools
  ]


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


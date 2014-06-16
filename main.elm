import Window
import Mouse
import Keyboard

import Vec
import Entity (..)
import Glyph
import Axes
import Camera
import Gpipeline

import Matrix

-- TODO Scene and tools overlay and cursor overlay should all be inserted in
-- different parts of the graphics transformation pipeline

type Scene = {
    camera: Camera.Camera
  , viewport: Camera.Viewport
  , cursor: Glyph.Glyph
  , axes: Axes.Axes
  , glyphTools: [Glyph.Glyph]
  }

initialCamera = initialEntity

initialViewport = initialEntity

initialScene = {
    camera = initialCamera
  , viewport = initialViewport
  , cursor = Glyph.openPawCursor
  , axes = Axes.initialAxes
  , glyphTools = [
      Glyph.scratchGlyph
    , Glyph.tentacleGlyph
    , Glyph.rectangleGlyph
    , Glyph.circGlyph
    , Glyph.clubGlyph
    , Glyph.heartGlyph
    , Glyph.diamondGlyph
    ]
  }

updateGlyph : AppInput -> Glyph.Glyph -> Glyph.Glyph
updateGlyph { dt } (entity, entityForm) =
  ({ entity |
     pos <- Vec.add entity.pos <| Vec.mulS entity.vel dt
   },
   entityForm)

addGlyph : AppInput -> Glyph.Glyph -> Glyph.Glyph
addGlyph { dt, mousePos, mouseDown } (entity, prevEntityForm) =
  let
    newEntityForm entity depth =
      group [
        prevEntityForm entity depth
      , move mousePos
        <| filled green <| ngon 5 20
      ]
  in
    (entity, newEntityForm)

updateGlyphTools : AppInput -> [Glyph.Glyph] -> [Glyph.Glyph]
updateGlyphTools appinput glyphTools =
  case appinput.mouseDown of
    True ->
      let
        headSet = take 0 glyphTools
        tailSet = drop 0 glyphTools
      in
        headSet ++ (
            addGlyph appinput <| head tailSet
          ) :: tail tailSet
    False ->
      map (\glyph ->
        updateGlyph appinput glyph
      ) glyphTools


updateCursor : AppInput -> Glyph.Glyph -> Glyph.Glyph
updateCursor { mousePos, mouseDown } _ =
  let (entity, entityForm) =
    if mouseDown == True then
      Glyph.closedPawCursor
    else
      Glyph.openPawCursor
  in
    ({ entity | pos <- mousePos }, entityForm)

updateCamera : AppInput -> Camera.Camera -> Camera.Camera
updateCamera { dt, keyDir } camera =
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
  let
    --mouseInViewport = Gpipeline.mouseInViewportFrame appInput.viewport appInput.mousePos
    --appInputInViewport = { appInput - mousePos | mousePos = mouseInViewport }

    stepCursor = updateCursor appInput scene.cursor

    mouseInCamera = Gpipeline.mouseInCameraFrame scene.camera appInput.mousePos
    appInputInCamera = { appInput - mousePos | mousePos = mouseInCamera }

    stepCamera = updateCamera appInputInCamera scene.camera
    stepGlyphTools = updateGlyphTools appInputInCamera scene.glyphTools

  in
    { scene | camera <- stepCamera
            , glyphTools <- stepGlyphTools
            , cursor <- stepCursor }

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

type AppInput = {
    dt: Time
  , mousePos: (Float, Float)
  , mouseDown: Bool
  , keyDir: { x: Int, y: Int }
  }


input : Signal AppInput
input =
  let
    mouseToFloat (x, y) = (toFloat x, toFloat y)
    relativeMouse (ox, oy) (x, y) = (x - ox, -(y - oy))
    center (w, h) = (div w 2, div h 2)

    clockInput : Signal Time
    clockInput = lift inSeconds (fps 30)

    mouseInput : Signal (Float, Float)
    mouseInput = sampleOn clockInput <| mouseToFloat
                   <~ (relativeMouse <~ (center <~ Window.dimensions) ~ Mouse.position)

    mouseDownInput = sampleOn clockInput Mouse.isDown

    keyInput = Keyboard.wasd
  in
    lift4 AppInput clockInput mouseInput mouseDownInput keyInput

windowDim = (1024, 600)
main = render <~ Window.dimensions ~ foldp updateScene initialScene input


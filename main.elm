import Window
import Mouse
import Keyboard

import Vec
import Entity (..)
import Glyph
import Axes
import Camera
import Gpipeline

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

updateViewport : AppInput -> Camera.Viewport -> Camera.Viewport
updateViewport { winDim } viewport =
  { viewport |
      dim <- tupInt2Float winDim
  }

updateScene : AppInput -> Scene -> Scene
updateScene appInput scene =
  let
    stepViewport = updateViewport appInput scene.viewport

    mouseInCamera = Gpipeline.mouseInCameraFrame stepViewport appInput.mousePos
    appInputInCamera = { appInput - mousePos | mousePos = mouseInCamera }

    stepCursor = updateCursor appInputInCamera scene.cursor

    mouseInWorld = Gpipeline.mouseInWorldFrame scene.camera appInputInCamera.mousePos
    appInputInWorld = { appInput - mousePos | mousePos = mouseInWorld }

    stepCamera = updateCamera appInputInWorld scene.camera
    stepGlyphTools = updateGlyphTools appInputInWorld scene.glyphTools

  in
    { scene | camera <- stepCamera
            , glyphTools <- stepGlyphTools
            , cursor <- stepCursor
            , viewport <- stepViewport }

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
    <| uncurry collage windowDim <| renderScene scene

----- All signals and input into the program

type AppInput = {
    dt: Time
  , mousePos: (Float, Float)
  , mouseDown: Bool
  , keyDir: { x: Int, y: Int }
  , winDim: (Int, Int)
  }


tupInt2Float (x, y) = (toFloat x, toFloat y)

input : Signal AppInput
input =
  let
    relativeMouse (ox, oy) (x, y) = (x - ox, -(y - oy))
    center (w, h) = (div w 2, div h 2)

    clockInput : Signal Time
    clockInput = lift inSeconds (fps 60)

    mouseInput : Signal (Float, Float)
    mouseInput = sampleOn clockInput <| tupInt2Float <~ Mouse.position

    --mouseDownInput : Signal Bool
    mouseDownInput = sampleOn clockInput Mouse.isDown

    --windowInput : Signal (Int, Int)
    windowInput = Window.dimensions

    --keyInput = Singal { x: Int, y: Int }
    keyInput = Keyboard.wasd
  in
    AppInput <~ clockInput ~ mouseInput ~ mouseDownInput ~ keyInput ~ windowInput

windowDim = (1024, 600)
main = render <~ Window.dimensions ~ foldp updateScene initialScene input


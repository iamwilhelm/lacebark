import Window
import Mouse
import Keyboard

import Vec
import Entity (..)
import Glyph
import Axes
import Camera
import Gpipeline

import Math.Matrix4
import Math.Vector3


-- config

windowDim = (1024, 600)

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

stampGlyph : AppInput -> Scene -> Scene
stampGlyph appInput scene =
  let
    headSet = take 0 scene.glyphTools
    tailSet = drop 0 scene.glyphTools
    stepGlyphTools = headSet ++ (addGlyph appInput <| head tailSet) :: tail tailSet
  in
    { scene | glyphTools <- stepGlyphTools }

updateGlyphTools : AppInput -> [Glyph.Glyph] -> [Glyph.Glyph]
updateGlyphTools appinput glyphTools =
  map (\glyph ->
    updateGlyph appinput glyph
  ) glyphTools

updateCursor : AppInput -> Glyph.Glyph -> Glyph.Glyph
updateCursor { mousePos, mouseDown } _ =
  let (entity, entityForm) =
    case mouseDown of
      True ->
        Glyph.closedPawCursor
      _ ->
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
      dim <- floatify winDim
  }

--updateScene : Event -> Scene -> Scene
updateScene appInput scene =
  let

    stepCursor = updateCursor (inCamera scene.viewport <| appInput) scene.cursor

    stepCamera = updateCamera
                   (inWorld scene.camera . inCamera scene.viewport <| appInput)
                   scene.camera

    stepGlyphTools = updateGlyphTools
                       (inWorld scene.camera . inCamera scene.viewport <| appInput)
                       scene.glyphTools


  in
    { scene | camera <- stepCamera
            , glyphTools <- stepGlyphTools
            , cursor <- stepCursor
    }

updateApp : AppInput -> Scene -> Scene
updateApp appInput scene =
  -- when click
  -- when trace (dragging while recording path)
  -- when drag
  -- when hover
  -- when move mouse
  let
    stepScene = { scene | viewport <- updateViewport appInput scene.viewport }
  in
    case appInput.mouseDown of
      True ->
        updateScene appInput . stampGlyph
          (inWorld stepScene.camera . inCamera stepScene.viewport <| appInput)
          <| stepScene
      False ->
        updateScene appInput stepScene


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


floatify (x, y) = (toFloat x, toFloat y)

input : Signal AppInput
input =
  let
    clockInput : Signal Time
    clockInput = lift inSeconds (fps 60)

    mouseInput : Signal (Float, Float)
    mouseInput =
      sampleOn clockInput
      <| floatify <~ Mouse.position

    --mouseDownInput : Signal Bool
    mouseDownInput =
      sampleOn clockInput
      <| dropRepeats Mouse.isDown

    --keyInput = Singal { x: Int, y: Int }
    keyInput =
      sampleOn clockInput Keyboard.wasd

    --windowInput : Signal (Int, Int)
    windowInput =
      sampleOn clockInput Window.dimensions
  in
    AppInput <~ clockInput ~ mouseInput ~ mouseDownInput ~ keyInput ~ windowInput

mouseInCameraFrame : Camera.Viewport -> (Float, Float) -> (Float, Float)
mouseInCameraFrame viewport mousePos =
  let
    center : (Float, Float) -> (Float, Float)
    center (w, h) = (w / 2, h / 2)

    offset = center viewport.dim

    v = Math.Matrix4.transform (
          Math.Matrix4.scale3 1 -1 1
          <| Math.Matrix4.translate3 -(fst offset) (snd offset) 0
          <| Math.Matrix4.makeRotate (degrees -viewport.rot) Math.Vector3.k
        )
        (Math.Vector3.vec3 (fst mousePos) (snd mousePos) 0)
  in
    (Math.Vector3.getX v, Math.Vector3.getY v)

inCamera : Camera.Viewport -> AppInput -> AppInput
inCamera viewport ({ mousePos } as appInput) =
  { appInput - mousePos | mousePos = mouseInCameraFrame viewport mousePos }

mouseInWorldFrame : Camera.Camera -> (Float, Float) -> (Float, Float)
mouseInWorldFrame camera mousePos =
  let
    v = Math.Matrix4.transform (
        Math.Matrix4.translate3 -(fst camera.pos) -(snd camera.pos) 0
        <| Math.Matrix4.makeRotate (degrees -camera.rot) Math.Vector3.k
      )
      (Math.Vector3.vec3 (fst mousePos) (snd mousePos) 0)
  in
    (Math.Vector3.getX v, Math.Vector3.getY v)

inWorld : Camera.Camera -> AppInput -> AppInput
inWorld camera ({ mousePos } as appInput) =
  { appInput - mousePos | mousePos = mouseInWorldFrame camera mousePos }



main = render <~ Window.dimensions ~ foldp updateApp initialScene input


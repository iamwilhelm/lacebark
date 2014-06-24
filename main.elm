import Window
import Util
import Vec
import Entity (..)
import Glyph
import Axes
import Camera
import Input
import Gpipeline
import Array
import Maybe (..)
import Drag

import Mouse

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
  --, selectedGlyph: 0
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
    --, Glyph.tentacleGlyph
    , Glyph.rectangleGlyph
    , Glyph.circGlyph
    --, Glyph.clubGlyph
    --, Glyph.heartGlyph
    --, Glyph.diamondGlyph
    ]
  }

-- updates to specific entity types

add5gonGlyph : Input.AppInput -> Glyph.Glyph -> Glyph.Glyph
add5gonGlyph { dt, mousePos, mouseDown } (entity, prevEntityForm) =
  let
    newEntityForm entity depth =
      group [
        prevEntityForm entity depth
      , move mousePos
        <| filled green <| ngon 5 20
      ]
  in
    (entity, newEntityForm)

add3gonGlyph : Input.AppInput -> Glyph.Glyph -> Glyph.Glyph
add3gonGlyph { dt, mousePos, mouseDown } (entity, prevEntityForm) =
  let
    newEntityForm entity depth =
      group [
        prevEntityForm entity depth
      , move mousePos
        <| filled purple <| ngon 3 20
      ]
  in
    (entity, newEntityForm)


addCircGlyph : Input.AppInput -> Glyph.Glyph -> Glyph.Glyph
addCircGlyph { dt, mousePos, mouseDown } (entity, prevEntityForm) =
  let
    newEntityForm entity depth =
      group [
        prevEntityForm entity depth
      , move mousePos
        <| filled yellow <| circle 20
      ]
  in
    (entity, newEntityForm)

addRectGlyph : Input.AppInput -> Glyph.Glyph -> Glyph.Glyph
addRectGlyph { dt, mousePos, mouseDown } (entity, prevEntityForm) =
  let
    newEntityForm entity depth =
      group [
        prevEntityForm entity depth
      , move mousePos
        <| filled red <| rect 10 10
      ]
  in
    (entity, newEntityForm)


updateGlyph : Input.AppInput -> Glyph.Glyph -> Glyph.Glyph
updateGlyph { dt } (entity, entityForm) =
  ({ entity |
     pos <- Vec.add entity.pos <| Vec.mulS entity.vel dt
   },
   entityForm)

updateCurrentGlyph : Input.AppInput -> Glyph.Glyph -> Glyph.Glyph
updateCurrentGlyph appInput ((entity, entityForm) as glyph) =
  case appInput.mouseEvent of
    Input.Move (Just (x, y)) ->
      glyph
    Input.MoveDrag (Just (x, y)) ->
      addRectGlyph appInput glyph
    Input.StartDrag (Just (x, y)) ->
      addCircGlyph appInput glyph
    Input.StopDrag (Just ((sx, sy), (ex, ey))) ->
      if sx == ex then
        add5gonGlyph appInput glyph
      else
        add3gonGlyph appInput glyph

updateGlyphTools : Input.AppInput -> [Glyph.Glyph] -> [Glyph.Glyph]
updateGlyphTools appInput glyphTools =
  Array.toList
  <| Array.indexedMap (\i glyph ->
    case i of
      0 ->
        updateGlyph appInput <| updateCurrentGlyph appInput glyph
      _ ->
        updateGlyph appInput glyph
  ) <| Array.fromList glyphTools

updateCursor : Input.AppInput -> Glyph.Glyph -> Glyph.Glyph
updateCursor { mousePos, mouseDown, mouseDragStart } _ =
  let (entity, entityForm) =
    case mouseDown of
      True ->
        Glyph.closedPawCursor
      False ->
        Glyph.openPawCursor
  in
    ({ entity | pos <- mousePos }, entityForm)

updateCamera : Input.AppInput -> Camera.Camera -> Camera.Camera
updateCamera { dt, keyDir } camera =
  let
    new_vec = Vec.mulS (toFloat keyDir.x, toFloat keyDir.y) -150
    new_pos = Vec.add camera.pos <| Vec.mulS camera.vel dt
  in
    { camera |
        pos <- new_pos
      , vel <- new_vec
    }

updateViewport : Input.AppInput -> Camera.Viewport -> Camera.Viewport
updateViewport { winDim } viewport =
  { viewport |
      dim <- Util.floatify winDim
  }

-- updates in frames

updateInViewportFrame (appInput, scene) =
  (
    appInput
  , { scene | viewport <- updateViewport appInput scene.viewport }
  )

updateInCameraFrame (appInput, scene) =
  let
    inFrameInput = Input.inCameraFrame scene.viewport appInput
  in
    (
      inFrameInput
    , { scene | cursor <- updateCursor inFrameInput scene.cursor }
    )

updateInWorldFrame (appInput, scene) =
  let
    inFrameInput = Input.inWorldFrame scene.camera appInput
  in
    (
      inFrameInput
    , { scene | camera <- updateCamera inFrameInput scene.camera
              , glyphTools <- updateGlyphTools inFrameInput scene.glyphTools
      }
    )
--updateScene : Event -> Scene -> Scene
updateScene appInput scene =
  snd
  <| updateInWorldFrame . updateInCameraFrame . updateInViewportFrame
  <| (appInput, scene)

-- when click
-- when trace (dragging while recording path)
-- when drag
-- when hover
-- when move mouse
updateApp : Input.AppInput -> Scene -> Scene
updateApp appInput scene =
  updateScene appInput scene


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


data Event = Tick Input.AppInput | Click Input.AppInput

event =
  merges [
    lift Click Input.appInput
  , lift Tick Input.appInput
  ]

main = render <~ Window.dimensions ~ foldp updateApp initialScene Input.appInput

--main = asText <~ Input.appInput

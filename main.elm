import Window
import Util
import Vec
import Entity (..)
import Glyph
import Axes
import Toolbar
import Camera
import Input
import Gpipeline
import Array
import Mouse

import Debug

-- config

windowDim = (1024, 600)

-- TODO Scene and tools overlay and cursor overlay should all be inserted in
-- different parts of the graphics transformation pipeline

type Scene = {
    camera: Camera.Camera
  , viewport: Camera.Viewport
  , cursor: Glyph.Glyph
  , axes: Axes.Axes
  , toolbar: Toolbar.Toolbar
  --, selectedGlyph: 0
  --, applyingGlyph: Nothing
  }

initialCamera = {
    pos = (0, 0)
  , vel = (0, 0)
  , rot = 0
  , dim = (10, 10)
  , radius = 75
  , colr = blue
  }

initialViewport = {
    pos = (0, 0)
  , vel = (0, 0)
  , rot = 0
  , dim = (10, 10)
  , radius = 75
  , colr = blue
  }

initialScene = {
    camera = initialCamera
  , viewport = initialViewport
  , cursor = Glyph.rectangleGlyph --Glyph.openPawCursor
  , axes = Axes.initialAxes
  , toolbar = Toolbar.initialToolbar
  }

-- updates to specific entity types

updateGlyph : Input.AppInput -> Glyph.Glyph -> Glyph.Glyph
updateGlyph { dt } ({ entity } as glyph) =
  { glyph | 
      entity <- { entity |
        pos <- Vec.add entity.pos <| Vec.mulS entity.vel dt
      }
  }

updateCurrentGlyph : Input.AppInput -> Glyph.Glyph -> Glyph.Glyph
updateCurrentGlyph appInput glyph =
  case appInput.mouseEvent of
    Input.Move (x, y) ->
      glyph
    Input.MoveDrag (x, y) ->
      Glyph.setPos glyph x y
    Input.StartDrag (x, y) ->
      glyph
    Input.StopDrag ((sx, sy), (ex, ey)) ->
      if (sx == ex) && (sy == ey) then
        Glyph.setPos glyph sx sy
      else
        glyph

updateGlyphTools : Input.AppInput -> Toolbar.Toolbar -> Toolbar.Toolbar
updateGlyphTools appInput toolbar =
  let
    glyphs = Array.toList
      <| Array.indexedMap (\i glyph ->
          case i of
            0 ->
              updateGlyph appInput <| updateCurrentGlyph appInput glyph
            _ ->
              updateGlyph appInput glyph
        )
      <| Array.fromList toolbar.glyphs
  in
    { toolbar | glyphs <- glyphs }

updateCursor : Input.AppInput -> Glyph.Glyph -> Glyph.Glyph
updateCursor { mousePos, mouseDown, mouseDragStart } ({ entity } as cursorGlyph) =
  let
    cursorGlyph =
      case mouseDown of
        True ->
          Glyph.closedPawCursor
        False ->
          Glyph.openPawCursor
    newEntity = cursorGlyph.entity
    updatedEntity = { entity | pos <- mousePos }
  in
    { cursorGlyph | entity <- updatedEntity }

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
  , 
    { scene |
      viewport <- updateViewport appInput scene.viewport
    }
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
    --c = Debug.log "camera" scene.camera
    --v = Debug.log "viewport" scene.viewport
    --a = Debug.log "axes" scene.axes
    --g = Debug.log "glyph" scene.toolbar

    inFrameInput = Input.inWorldFrame scene.camera appInput
  in
    (
      inFrameInput
    , { scene | camera <- updateCamera inFrameInput scene.camera
              , toolbar <- updateGlyphTools inFrameInput scene.toolbar
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
    Toolbar.transform windowDim <| Toolbar.draw scene.toolbar
  ]
  <| Gpipeline.renderInWorldFrame (Toolbar.selectedGlyph scene.toolbar) [
  ]
  <| [
    Axes.draw scene.axes
  , Glyph.draw <| Toolbar.selectedGlyph scene.toolbar
  ]


render : (Int, Int) -> Scene -> Element
render (w, h) scene =
  color lightGray
    <| container w h middle
    <| color gray
    <| uncurry collage windowDim <| renderScene scene

main = render <~ Window.dimensions ~ foldp updateApp initialScene Input.appInput
--main = asText <~ Input.appInput


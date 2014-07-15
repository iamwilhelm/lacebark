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
  , glyphTools: [Glyph.Glyph]
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
  , glyphTools = [
    --  Glyph.scratchGlyph
    --, Glyph.tentacleGlyph
      Glyph.rectangleGlyph
    --, Glyph.circGlyph
    --, Glyph.clubGlyph
    --, Glyph.heartGlyph
    --, Glyph.diamondGlyph
    ]
  }

-- updates to specific entity types

updateGlyph : Input.AppInput -> Glyph.Glyph -> Glyph.Glyph
updateGlyph { dt } ({ entity } as glyph) =
  { glyph | 
      entity <- { entity |
        pos <- Vec.add entity.pos <| Vec.mulS entity.vel dt
      }
  }

highlightGlyph : Glyph.Glyph -> Glyph.Glyph
highlightGlyph glyph =
  Glyph.setColr glyph yellow

updateCurrentGlyph : Input.AppInput -> Glyph.Glyph -> Glyph.Glyph
updateCurrentGlyph appInput glyph =
  let
    --h = Debug.log "glyph" glyph
    h = 3
  in
    case appInput.mouseEvent of
      Input.Move (x, y) ->
        glyph
      Input.MoveDrag (x, y) ->
        Glyph.setDim glyph (toFloat x - 100) -(toFloat y - 100)
        --glyph
      Input.StartDrag (x, y) ->
        glyph
      Input.StopDrag ((sx, sy), (ex, ey)) ->
        if (sx == ex) && (sy == ey) then
          highlightGlyph glyph
        else
          glyph

updateGlyphTools : Input.AppInput -> [Glyph.Glyph] -> [Glyph.Glyph]
updateGlyphTools appInput glyphTools =
  Array.toList
  <| Array.indexedMap (\i glyph ->
      case i of
        0 ->
          updateGlyph appInput <| updateCurrentGlyph appInput glyph
        _ ->
          updateGlyph appInput glyph
    )
  <| Array.fromList glyphTools

updateCursor : Input.AppInput -> Glyph.Glyph -> Glyph.Glyph
updateCursor { mousePos, mouseDown, mouseDragStart } ({ entity } as cursorGlyph) =
  let
    cursorGlyph =
      case mouseDown of
        True ->
          --Glyph.closedPawCursor
          Glyph.rectangleGlyph
        False ->
          --Glyph.openPawCursor
          Glyph.rectangleGlyph
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
    --g = Debug.log "glyph" scene.glyphTools

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
    Axes.draw scene.axes
  ]
  <| [
    Glyph.draw <| head scene.glyphTools
  ]


render : (Int, Int) -> Scene -> Element
render (w, h) scene =
  color lightGray
    <| container w h middle
    <| color gray
    <| uncurry collage windowDim <| renderScene scene

main = render <~ Window.dimensions ~ foldp updateApp initialScene Input.appInput

--main = asText <~ Input.appInput


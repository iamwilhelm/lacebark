module Input where

import Window
import Mouse
import Keyboard
import Camera

import Util
import Math.Matrix4
import Math.Vector3
import Drag
import Maybe (..)

----- All signals and input into the program

data MouseEvent = Move (Float, Float)
                | MoveDrag (Float, Float)
                | StartDrag (Float, Float)
                | StopDrag ((Float, Float), (Float, Float))

type AppInput = {
    dt: Time
  , mousePos: (Float, Float)
  , mouseDown: Bool
  , mouseDragStart: (Float, Float)
  , mouseDragStop: ((Float, Float), (Float, Float))
  , mouseEvent: MouseEvent
  , keyDir: { x: Int, y: Int }
  , winDim: (Int, Int)
  }

appInput : Signal AppInput
appInput =
  let
    clockInput : Signal Time
    clockInput = lift inSeconds (fps 15)

    mouseInput : Signal (Float, Float)
    mouseInput =
      sampleOn clockInput
      <| Util.floatify <~ Mouse.position

    mouseDownInput : Signal Bool
    mouseDownInput =
      sampleOn clockInput <| Mouse.isDown

    mouseDragStart : Signal (Float, Float)
    mouseDragStart =
      sampleOn (dropRepeats Mouse.isDown)
      <| sampleOn clockInput <| Util.floatify <~ Mouse.position

    mouseDragStop : Signal ((Float, Float), (Float, Float))
    mouseDragStop =
      let start = keepWhen Mouse.isDown (0, 0) mouseDragStart
          stop = sampleOn clockInput <| Util.floatify <~ Mouse.position
      in
        (,) <~ start ~ stop |> sampleOn (dropRepeats Mouse.isDown)

    -- TODO transform mouse position in mouse reverse transformations
    mouseEvent =
      merges [
        lift Move <| dropWhen Mouse.isDown (0.0, 0.0)
                  <| sampleOn clockInput <| Util.floatify <~ Mouse.position
      , lift MoveDrag <| keepWhen Mouse.isDown (0.0, 0.0)
                      <| sampleOn clockInput <| Util.floatify <~ Mouse.position
      , lift StartDrag <| keepWhen Mouse.isDown (0.0, 0.0) mouseDragStart
      , lift StopDrag mouseDragStop
      ]
    --keyInput = Signal { x: Int, y: Int }
    keyInput =
      sampleOn clockInput Keyboard.wasd

    windowInput : Signal (Int, Int)
    windowInput =
      sampleOn clockInput Window.dimensions
  in
    AppInput <~ clockInput ~ mouseInput ~ mouseDownInput
              ~ mouseDragStart ~ mouseDragStop ~ mouseEvent
              ~ keyInput ~ windowInput

inCameraFrame : Camera.Viewport -> AppInput -> AppInput
inCameraFrame viewport ({ mousePos, mouseEvent } as appInput) =
  let
    newMouseEvent =
      case mouseEvent of
        Move pos ->
          Move <| mouseInCameraFrame viewport pos
        MoveDrag pos ->
          MoveDrag <| mouseInCameraFrame viewport pos
        StartDrag pos ->
          StartDrag <| mouseInCameraFrame viewport pos
        StopDrag (startPos, stopPos) ->
          StopDrag <| (,)
            (mouseInCameraFrame viewport startPos)
            (mouseInCameraFrame viewport stopPos)
  in
    { appInput | mousePos <- mouseInCameraFrame viewport mousePos
      , mouseEvent <- newMouseEvent
    }


inWorldFrame : Camera.Camera -> AppInput -> AppInput
inWorldFrame camera ({ mousePos } as appInput) =
  { appInput - mousePos | mousePos = mouseInWorldFrame camera mousePos }

--- helper functions

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




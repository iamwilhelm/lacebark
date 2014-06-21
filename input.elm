module Input where

import Window
import Mouse
import Keyboard
import Camera

import Util
import Math.Matrix4
import Math.Vector3
--import Drag

----- All signals and input into the program

type AppInput = {
    dt: Time
  , mousePos: (Float, Float)
  , mouseDown: Bool
  , keyDir: { x: Int, y: Int }
  , winDim: (Int, Int)
  }


appInput : Signal AppInput
appInput =
  let
    clockInput : Signal Time
    clockInput = lift inSeconds (fps 60)

    mouseInput : Signal (Float, Float)
    mouseInput =
      sampleOn clockInput
      <| Util.floatify <~ Mouse.position

    --mouseDownInput : Signal Bool
    mouseDownInput =
      sampleOn clockInput
      <| dropRepeats Mouse.isDown

    --mouseDragStart = Drag.start
    --mouseDragDrop = Drag.drop

    --keyInput = Singal { x: Int, y: Int }
    keyInput =
      sampleOn clockInput Keyboard.wasd

    --windowInput : Signal (Int, Int)
    windowInput =
      sampleOn clockInput Window.dimensions
  in
    AppInput <~ clockInput ~ mouseInput ~ mouseDownInput ~ keyInput ~ windowInput

inCameraFrame : Camera.Viewport -> AppInput -> AppInput
inCameraFrame viewport ({ mousePos } as appInput) =
  { appInput - mousePos | mousePos = mouseInCameraFrame viewport mousePos }


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




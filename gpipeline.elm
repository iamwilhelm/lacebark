----- Graphics pipeline
-- Frame coords
-- Glyph coords
-- World coords
-- Camera coords
-- Viewport coords
-- Window coords

module Gpipeline where

import Window
import Transform2D
import Vec
import Entity (..)
import Glyph
import Camera
import Math.Matrix4
import Math.Vector3

glyph2WorldMat : Glyph.Glyph -> Transform2D.Transform2D
glyph2WorldMat (entity, entityForm) =
  Transform2D.multiply
    (uncurry Transform2D.translation entity.pos)
    (Transform2D.rotation (degrees entity.rot))

-- [Forms in this frame already] ->
-- [Forms that need to be transformed to this frame] ->
-- [Resulting Forms in this frame]
renderInWorldFrame : Glyph.Glyph -> [Form] -> [Form] -> [Form]
renderInWorldFrame glyph inFrameForms childFrameForms =
  groupTransform (glyph2WorldMat glyph) childFrameForms :: inFrameForms

world2CameraMat : Camera.Camera -> Transform2D.Transform2D
world2CameraMat camera =
  Transform2D.multiply
    (uncurry Transform2D.translation camera.pos)
    (Transform2D.rotation (degrees camera.rot))

renderInCameraFrame : Camera.Camera -> [Form] -> [Form] -> [Form]
renderInCameraFrame camera inFrameForms childFrameForms =
  -- TODO clip all objects outside the camera
  groupTransform (world2CameraMat camera) childFrameForms :: inFrameForms

camera2ViewportMat : Transform2D.Transform2D
camera2ViewportMat =
  Transform2D.identity

renderInViewportFrame : [Form] -> [Form] -> [Form]
renderInViewportFrame inFrameForms childFrameForms =
  -- TODO clip all objects outside the viewport
  groupTransform camera2ViewportMat childFrameForms :: inFrameForms

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



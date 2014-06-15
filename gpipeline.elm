----- Graphics pipeline
-- Frame coords
-- Glyph coords
-- World coords
-- Camera coords
-- Viewport coords
-- Window coords

module Gpipeline where

import Transform2D
import Entity (..)
import Glyph
import Camera

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


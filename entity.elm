module Entity where

import Vec

type Entity = {
    pos: Vec.Vec
  , vel: Vec.Vec
  , rot: Float
  , colr: Color
  , dim: Vec.Vec
  , radius: Float
  }

initializeEntity = {
    pos = (0, 0)
  , vel = (0, 0)
  , rot = 0
  , dim = (10, 10)
  , radius = 75
  , colr = blue
  }

initialEntity = {
    pos = (0, 0)
  , vel = (0, 0)
  , rot = 0
  , dim = (10, 10)
  , radius = 75
  , colr = blue
  }

setColr : Entity -> Color -> Entity
setColr entity newColr =
  { entity | colr <- newColr }

setPos : Entity -> Float -> Float -> Entity
setPos entity x y =
  { entity | pos <- (x, y) }


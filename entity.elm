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

initialEntity = {
    pos = (0, 0)
  , vel = (0, 0)
  , rot = 0
  , dim = (10, 10)
  , radius = 75
  , colr = blue
  }


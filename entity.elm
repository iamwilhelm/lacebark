module Entity where

import Vec

type Entity = {
    pos: Vec.Vec
  , vel: Vec.Vec
  , rad: Float
  , colr: Color
  , dim: Vec.Vec
  , radius: Float
  }

initialEntity = { pos = (0, 0)
                , vel = (0, 0)
                , rad = 0

                , colr = blue
                , dim = (10, 10)
                , radius = 75
                }


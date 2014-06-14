module Axes where

import Entity (..)

type Axes = Entity
initialAxes = initialEntity

data AxesDirection = XAxis | YAxis

drawTicks : Axes -> AxesDirection -> Form
drawTicks axes direction =
  group <| map (\x ->
      case direction of
        XAxis ->
          traced (solid black) (segment (x, 10) (x, -10))
        YAxis ->
          traced (solid black) (segment (10, x) (-10, x))
    )
    [-500, -400, -300, -200, -100, 100, 200, 300, 400, 500]

draw : Axes -> Form
draw axes =
  group [
    traced (solid black) (segment (0, -5000) (0, 5000))
  , drawTicks axes XAxis
  , traced (solid black) (segment (5000, 0) (-5000, 0))
  , drawTicks axes YAxis
  ]


import Window
import Mouse

-- center : (Float, Float) -> (Float, Float)
center (w, h) = (div w 2 , div h 2)
relativeMouse (ox, oy) (x, y) = (x - ox, -(y - oy))
cursor = filled darkCharcoal (rect 10 10)

input = (,) <~ lift inSeconds (fps 30)
             ~ lift2 relativeMouse (center <~ Window.dimensions) Mouse.position

controlScene (mouseX, mouseY) =
  [ move (toFloat mouseX, toFloat mouseY)
      cursor

  , move (toFloat -350, toFloat 290)
      (toForm (asText (mouseX, mouseY)))
  ]

toolView scene =
  [scale 0.25 <| group scene]

toolbar mouse =
  flow down [
    collage 80 80 <| toolView <| myScene mouse
  , image 80 80 "./yogi.jpg"
  ]

render (w, h) (dt, mouse) =
  color lightGray <| container w h middle
    <| flow right [
         toolbar mouse
       , color gray
           <| collage 1024 600 ((myScene mouse) ++ (controlScene mouse))
       ]

areaRect w h = w * h

rectangle w h colr = 
  group [
    filled colr (rect w h),
    toForm (asText (areaRect w h))
  ]

myScene (mouseX, mouseY) =
  [ --outlined (dashed blue) (ngon 5 300)
    rotate (degrees 45)
      <| move (0, 0)
        <| rectangle 100 100 red

  , move (30, 30)
      <| filled red <| circle 50

  , move (-30, 30)
      <| filled red <| circle 50

  , move (toFloat mouseX, toFloat mouseY)
      (rectangle 50 50 green)
  --, toForm (asText "hello world")
  ]


main = lift2 render Window.dimensions input


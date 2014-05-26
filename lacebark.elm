import Window
import Mouse

center (w, h) = (div w 2 , div h 2)
relativeMouse (ox, oy) (x, y) = (x - ox, -(y - oy))
cursor = filled darkCharcoal (rect 10 10)

input = (,) <~ lift inSeconds (fps 30)
             ~ lift2 relativeMouse (center <~ Window.dimensions) Mouse.position


areaRect w h = w * h

rectangle w h colr = 
  group [
    filled colr (rect w h),
    toForm (asText (areaRect w h))
  ]

render (w, h) (dt, mouse) = color lightGray
                              <| container w h  middle
                              <| color gray
                              <| collage 1024 600 (myScene ++ (controlScene mouse))

controlScene (mouseX, mouseY) =
  [ move (toFloat mouseX, toFloat mouseY)
      cursor

  , move (toFloat -350, toFloat 290)
      (toForm (asText (mouseX, mouseY)))
  ]

myScene =
  [ outlined (dashed blue) (ngon 5 300)
  , filled red (circle 75)
  , rotate (degrees 60)
      (move (0, 0)
        (rectangle 100 100 yellow))
  , move (40, 40)
      (rectangle 80 80 red)

  --, toForm (asText "hello world")
  ]


main = lift2 render Window.dimensions input


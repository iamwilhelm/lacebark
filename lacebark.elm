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
  flow down <| map (\scene -> collage 80 80 <| toolView <| scene mouse) scenes

render (w, h) (dt, mouse) =
  color lightGray <| container w h middle
    <| flow right [
         toolbar mouse
       , color gray
           <| collage 1024 600 <| (head scenes) mouse
       ]

areaRect w h = w * h

rectangle w h colr = 
  group [
    filled colr (rect w h),
    toForm (asText (areaRect w h))
  ]

clubScene (mouseX, mouseY) =
  [
    move (0, 60)
      <| filled charcoal <| circle 40
  , move (35, 0)
      <| filled charcoal <| circle 40
  , move (-35, 0)
      <| filled charcoal <| circle 40
  , move (0, -35)
      <| filled charcoal <| rect 20 80
  ]

heartScene (mouseX, mouseY) =
  [ --outlined (dashed blue) (ngon 5 300)
    move (30, 30)
      <| filled red <| circle 50

  , move (-30, 30)
      <| filled red <| circle 50

  , rotate (degrees 45)
      <| move (0, 0)
        <| rectangle 100 100 red
  ]

diamondScene (mouseX, mouseY) =
  [
    filled red <| ngon 4 80
  ]

includeScene scene =
  group scene

recursiveIncludeScene scene transform transform2 depth =
  group
    <| ([scene]
          ++ case depth of
               1 ->
                 []
               _ ->
                 [
                   transform
                     (recursiveIncludeScene scene transform transform2 (depth - 1))
                 , transform2
                     (recursiveIncludeScene scene transform transform2 (depth - 1))
                 ]
       )

scratchScene (mouseX, mouseY) =
  [
    recursiveIncludeScene
      --(group <| diamondScene (mouseX, mouseY))
      (filled red <| ngon 4 40)
      (move (toFloat mouseX / 3, toFloat mouseY / 3) . rotate (degrees <| toFloat mouseX) . scale 0.8)
      (move (toFloat mouseX / 3 - 80, toFloat mouseY / 3 - 80) . rotate (degrees <| toFloat mouseY) . scale 0.7)
      8

  -- , move (toFloat mouseX, toFloat mouseY)
  --     (rectangle 50 50 green)
  --, toForm (asText "hello world")

  ]

scenes =
  [ scratchScene
  , heartScene
  , clubScene
  , diamondScene
  ]


main = lift2 render Window.dimensions input

--    group [
--      filled red <| ngon 4 80
--    , move (50, 50)
--        <| scale (0.9)
--        <| rotate (degrees 30)
--        <| group [
--             filled red <| ngon 4 80
--           , move (50, 50)
--              <| scale (0.9)
--              <| rotate (degrees 30)
--              <| group [
--                   filled red <| ngon 4 80
--                 , move (50, 50)
--                   <| scale (0.9)
--                   <| rotate (degrees 30)
--                   <| group [
--                        filled red <| ngon 4 80
--                      ]
--                 ]
--           ]
--    ]



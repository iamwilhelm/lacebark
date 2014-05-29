import Window
import Mouse

type Vec = (Float, Float)

vecAdd : Vec -> Vec -> Vec
vecAdd (ax, ay) (bx, by) = (ax + bx, ay + by)

vecMulS : Vec -> Float -> Vec
vecMulS (x, y) a = (x * a, y * a)


center : (Int, Int) -> (Int, Int)
center (w, h) = (div w 2 , div h 2)

relativeMouse : (Int, Int) -> (Int, Int) -> (Int, Int)
relativeMouse (ox, oy) (x, y) = (x - ox, -(y - oy))
cursor = filled darkCharcoal (rect 10 10)

input = (,) <~ lift inSeconds (fps 30)
             ~ lift2 relativeMouse (center <~ Window.dimensions) Mouse.position


type Glyph = { pos: Vec, vel: Vec, rad: Float, col:Color, forms:Form }

initialGlyph = { pos = (0, 0)
               , vel = (0, -1)
               , rad = 0
               , col = red
               , forms = group [ filled red <| ngon 5 40 ]
               }

diamondGlyph = { initialGlyph |
  forms <- group [ filled red <| ngon 4 80 ] }

clubGlyph = {
  initialGlyph |
    forms <- group [
               move (0, 60)
                 <| filled charcoal <| circle 40
             , move (35, 0)
                 <| filled charcoal <| circle 40
             , move (-35, 0)
                 <| filled charcoal <| circle 40
             , move (0, -35)
                 <| filled charcoal <| rect 20 80
             ] }

updateGlyph : Time -> Glyph -> Glyph
updateGlyph dt s = { s | pos <- vecAdd s.pos <| vecMulS s.vel dt }



render : (Int, Int) -> Glyph -> Element
render (w, h) shape =
  -- let mainScene scene = scene.rootGlyph
  --     scenes = [mainScene initialScene]
  let renderGlyph {rad, col, pos, forms} = move pos <| forms
      shapes = [renderGlyph shape]
  in  color lightGray
        <| container w h middle
        <| flow right [
             -- toolbar mouse
             color gray <| collage 1024 600 <| shapes
           ]


-- initialGlyphTools = [ scratchGlyph, diamondGlyph, clubGlyph ]
-- 
-- -- a scene shows a shape. Each shape can be composed of many other shapes or itself 
-- -- Glyphs are of type Form
-- initialScene = { camera = []
--                , shapeTools = initialGlyphTools
--                , rootGlyph = head initialGlyphTools
--                }
-- 
-- updateScene : Time -> Scene -> Scene
-- updateScene dt s = { s | }

--scratchGlyph =
--  group [
--    diamondGlyph
--
--  --, includeGlyph
--  --    (filled red <| ngon 4 40)
--  --    (move (toFloat mouseX / 3, toFloat mouseY / 3) . rotate (degrees <| toFloat mouseX) . scale 0.8)
--  --    8
--
--  --, recursiveIncludeScene
--  --    (filled red <| ngon 4 40)
--  --    (move (toFloat mouseX / 3 - 80, toFloat mouseY / 3 - 80) . rotate (degrees <| toFloat mouseY) . scale 0.7)
--  --    8
--  ]


clock = lift inSeconds (fps 30)
main = render <~ Window.dimensions ~ foldp updateGlyph initialGlyph clock



--controlScene (mouseX, mouseY) =
--  [ move (toFloat mouseX, toFloat mouseY)
--      cursor
--
--  , move (toFloat -350, toFloat 290)
--      (toForm (asText (mouseX, mouseY)))
--  ]
--
--toolView scene =
--  [scale 0.25 <| group scene]
--
--toolbar mouse =
--  flow down <| map (\scene -> collage 80 80 <| toolView <| scene mouse) scenes
--
--clubScene (mouseX, mouseY) =
--  [
--    move (0, 60)
--      <| filled charcoal <| circle 40
--  , move (35, 0)
--      <| filled charcoal <| circle 40
--  , move (-35, 0)
--      <| filled charcoal <| circle 40
--  , move (0, -35)
--      <| filled charcoal <| rect 20 80
--  ]
--
--heartScene (mouseX, mouseY) =
--  [ --outlined (dashed blue) (ngon 5 300)
--    move (30, 30)
--      <| filled red <| circle 50
--
--  , move (-30, 30)
--      <| filled red <| circle 50
--
--  , rotate (degrees 45)
--      <| move (0, 0)
--        <| filled red
--          <| rect 100 100
--
--  ]
--
--diamondScene (mouseX, mouseY) =
--  [
--    filled red <| ngon 4 80
--  ]
--
--includeScene scene =
--  group scene
--
--recursiveIncludeScene scene transform transform2 depth =
--  group
--    <| ([scene]
--          ++ case depth of
--               1 ->
--                 []
--               _ ->
--                 [
--                   transform
--                     (recursiveIncludeScene scene transform transform2 (depth - 1))
--                 , transform2
--                     (recursiveIncludeScene scene transform transform2 (depth - 1))
--                 ]
--       )
--
--scratchScene (mouseX, mouseY) =
--  [
--    recursiveIncludeScene
--      --(group <| diamondScene (mouseX, mouseY))
--      (filled red <| ngon 4 40)
--      (move (toFloat mouseX / 3, toFloat mouseY / 3) . rotate (degrees <| toFloat mouseX) . scale 0.8)
--      (move (toFloat mouseX / 3 - 80, toFloat mouseY / 3 - 80) . rotate (degrees <| toFloat mouseY) . scale 0.7)
--      8
--
--  -- , move (toFloat mouseX, toFloat mouseY)
--  --     (rectangle 50 50 green)
--  --, toForm (asText "hello world")
--
--  ]
--
--scenes =
--  [ scratchScene
--  , heartScene
--  , clubScene
--  , diamondScene
--  ]




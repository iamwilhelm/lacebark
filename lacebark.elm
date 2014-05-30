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


-- a glyph is a combination of shapes. glyph can be composed of many other shapes or itself 
type Glyph = { pos: Vec, vel: Vec, rad: Float, colr:Color, formGroup:Form }

initialGlyph = { pos = (0, 0)
               , vel = (0, 0)
               , rad = 0
               , colr = blue
               , formGroup = group [ filled red <| ngon 5 40 ]
               }

clubGlyph = { initialGlyph |
    formGroup <- group [
                   move (0, 60)
                     <| filled charcoal <| circle 40
                 , move (35, 0)
                     <| filled charcoal <| circle 40
                 , move (-35, 0)
                     <| filled charcoal <| circle 40
                 , move (0, -35)
                     <| filled charcoal <| rect 20 80
                 ] }

heartGlyph = { initialGlyph |
    formGroup <- group [
                   move (30, 30)
                     <| filled red <| circle 50

                 , move (-30, 30)
                     <| filled red <| circle 50

                 , rotate (degrees 45)
                     <| move (0, 0)
                       <| filled red
                         <| rect 100 100
                 ] }


diamondGlyph = { initialGlyph |
    formGroup <- group [ filled red <| ngon 4 80 ] }

initialGlyphTools = [ initialGlyph, clubGlyph, heartGlyph, diamondGlyph ]


type Scene = { camera:Float, rootGlyph:Glyph, glyphTools:[Glyph] }
initialScene = { camera = 0
               , glyphTools = initialGlyphTools
               , rootGlyph = head initialGlyphTools
               }


updateGlyph : Time -> (Int, Int) -> Glyph -> Glyph
updateGlyph dt (mouseX, mouseY) g = { g | pos <- vecAdd g.pos <| vecMulS g.vel dt }

updateGlyphTools : Time -> (Int, Int) -> [Glyph] -> [Glyph]
updateGlyphTools dt (mouseX, mouseY) glyphTools =
  map (\glyph -> updateGlyph dt (mouseX, mouseY) glyph) glyphTools

updateScene : (Time, (Int, Int)) -> Scene -> Scene
updateScene (dt, mouse) s = { s | rootGlyph <- updateGlyph dt mouse s.rootGlyph
                                , glyphTools <- updateGlyphTools dt mouse s.glyphTools }


render : (Int, Int) -> Scene -> Element
render (w, h) scene =
  let renderGlyph {rad, colr, pos, formGroup} =
        move pos <| formGroup
      renderScene {camera, rootGlyph, glyphTools} =
        [ renderGlyph rootGlyph ]
      renderToolbar {camera, glyphTools} =
        flow down <| map (\glyph -> collage 80 80 [scale 0.2 <| renderGlyph glyph]) glyphTools
  in  color lightGray
        <| container w h middle
        <| flow right [
             renderToolbar scene
           , color gray <| collage 1024 600 <| renderScene scene
           ]


clock = lift inSeconds (fps 30)
input = (,) <~ clock
             ~ lift2 relativeMouse (center <~ Window.dimensions) Mouse.position

main = render <~ Window.dimensions ~ foldp updateScene initialScene input


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


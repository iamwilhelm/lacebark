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

-- a hack to correct for width of toolbar
toolbarAdjust: (Int, Int) -> (Int, Int)
toolbarAdjust (x, y) = (x + 80, y)


-- a glyph is a combination of shapes. glyph can be composed of many other shapes or itself 
type Entity = { pos: Vec, vel: Vec, rad: Float, colr: Color, dim: Vec, radius: Float }
type Glyph = (Entity, Entity -> Form)

initialEntity = { pos = (0, 0)
                , vel = (0, 0)
                , rad = 0

                , colr = blue
                , dim = (10, 10)
                , radius = 75
                }

arrowEntity = initialEntity

arrowEntityForm : Entity -> Form
arrowEntityForm glyph =
  scale 0.4
  <| group [
       filled black <| ngon 3 20
     , move (8.25, -15)
       <| rotate (degrees 30)
       <| filled black <| rect 10 25
     ]

arrowGlyph = (arrowEntity, arrowEntityForm)


handEntity = initialEntity

handEntityForm : Entity -> Form
handEntityForm glyph =
  scale 0.4
  <| group [
       filled black <| circle 20
     ]

handGlyph = (handEntity, handEntityForm)


rectangleEntity = { initialEntity |
    colr <- orange
  , dim <- (120, 120) }

rectangleEntityForm : Entity -> Form
rectangleEntityForm glyph =
  group [
    filled glyph.colr <| uncurry rect glyph.dim
  ]

rectangleGlyph = (rectangleEntity, rectangleEntityForm)


circleEntity = { initialEntity |
    colr <- red
  , radius <- 60 }

circleEntityForm : Entity -> Form
circleEntityForm glyph =
  group [
    filled glyph.colr <| circle glyph.radius
  ]

circleGlyph = (circleEntity, circleEntityForm)



--clubGlyph = { initialGlyph |
--    formGroup <- group [
--                   move (0, 60)
--                     <| filled charcoal <| circle 40
--                 , move (35, 0)
--                     <| filled charcoal <| circle 40
--                 , move (-35, 0)
--                     <| filled charcoal <| circle 40
--                 , move (0, -35)
--                     <| filled charcoal <| rect 20 80
--                 ] }
--
--heartGlyph = { initialGlyph |
--    formGroup <- group [
--                   move (30, 30)
--                     <| filled red <| circle 50
--
--                 , move (-30, 30)
--                     <| filled red <| circle 50
--
--                 , rotate (degrees 45)
--                     <| move (0, 0)
--                       <| filled red
--                         <| rect 100 100
--                 ] }
--
--diamondGlyph = { initialGlyph |
--    formGroup <- group [ filled red <| ngon 4 80 ] }
--
--scratchGlyph = { initialGlyph |
--    formGroup <- group [
--                   include clubGlyph
--                 ,
--                   move (80, 80)
--                     <| include heartGlyph
--                 ] }
--
--include : Glyph -> Form
--include glyph = move glyph.pos glyph.formGroup



type Scene = { camera: Float, glyphTools: [Glyph], cursor: Glyph }
initialScene = { camera = 0
               , glyphTools = [ rectangleGlyph, circleGlyph ] --, scratchGlyph, circleGlyph, clubGlyph, heartGlyph, diamondGlyph ]
               , cursor = arrowGlyph
               }


updateGlyph : Time -> (Int, Int) -> Glyph -> Glyph
updateGlyph dt (mouseX, mouseY) (entity, entityForm) = ({ entity |
    pos <- vecAdd entity.pos <| vecMulS entity.vel dt
  },
  entityForm)

updateGlyphTools : Time -> (Int, Int) -> [Glyph] -> [Glyph]
updateGlyphTools dt (mouseX, mouseY) glyphTools =
  map (\glyph -> updateGlyph dt (mouseX, mouseY) glyph) glyphTools

updateCursor : Time -> (Int, Int) -> Bool -> Glyph -> Glyph
updateCursor dt (mouseX, mouseY) mouseDown (entity, entityForm) =
  ({ entity | pos <- (toFloat mouseX, toFloat mouseY) }, entityForm)

updateScene : (Time, (Int, Int), Bool) -> Scene -> Scene
updateScene (dt, mouse, mouseDown) scene =
  let cur = case mouseDown of
            True ->
              handGlyph
            False ->
              arrowGlyph
  in { scene | glyphTools <- updateGlyphTools dt mouse scene.glyphTools
             , cursor <- updateCursor dt mouse mouseDown cur }



renderGlyph (entity, entityForm) =
  move entity.pos <| entityForm entity

renderScene scene =
  [ renderGlyph (head scene.glyphTools), renderGlyph scene.cursor ]

renderToolbar { glyphTools } =
  flow down
  <| map (\glyph -> collage 80 80 [scale 0.2 <| renderGlyph glyph ]) glyphTools

render : (Int, Int) -> Scene -> Element
render (w, h) scene =
  color lightGray
    <| container w h middle
    <| flow right [
         renderToolbar scene
       , color gray <| collage 1024 600 <| renderScene scene
       ]


clock = lift inSeconds (fps 30)
input = (,,) <~ clock
             ~ lift2 relativeMouse (center <~ (toolbarAdjust <~ Window.dimensions)) Mouse.position
             ~ Mouse.isDown

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


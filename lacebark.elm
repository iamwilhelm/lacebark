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
type Glyph = (Entity, Entity -> Int -> Form)

initialEntity = { pos = (0, 0)
                , vel = (0, 0)
                , rad = 0

                , colr = blue
                , dim = (10, 10)
                , radius = 75
                }



rectangle : Float -> Float -> Color -> Glyph
rectangle w h colr =
  let
    entity = { initialEntity |
                 colr <- colr
               , dim <- (w, h) }

    entityForm glyph depth =
      group [
        filled glyph.colr <| uncurry rect glyph.dim
      ]
  in
    (entity, entityForm)

rectangleGlyph = rectangle 120 120 blue



circ : Float -> Color -> Glyph
circ radius colr =
  let
    entity = { initialEntity |
        colr <- red
      , radius <- 60 }

    entityForm glyph depth =
      group [
        filled glyph.colr <| circle glyph.radius
      ]
  in
    (entity, entityForm)

circleGlyph = circ 60 red



arrowEntity = initialEntity

arrowEntityForm : Entity -> Int -> Form
arrowEntityForm glyph depth =
  scale 0.4
  <| group [
       filled black <| ngon 3 20
     , move (8.25, -15)
       <| rotate (degrees 30)
       <| filled black <| rect 10 25
     ]

arrowGlyph = (arrowEntity, arrowEntityForm)


handEntity = initialEntity

handEntityForm : Entity -> Int -> Form
handEntityForm glyph depth =
  scale 0.4
  <| group [
       filled black <| circle 20
     ]

handGlyph = (handEntity, handEntityForm)




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

include : Glyph -> Form
include (entity, entityForm) =
  -- renderGlyph (entity entityForm)
  move entity.pos <| entityForm entity 10

rinclude : Glyph -> Int -> Form
rinclude (entity, entityForm) depth =
  case depth of
    0 ->
      group []
    _ ->
      group [ entityForm entity (depth - 1) ]


scratchEntity = { initialEntity |
  colr <- yellow }

scratchEntityForm : Entity -> Int -> Form
scratchEntityForm entity depth =
  group [
    include rectangleGlyph
  ,
    rotate (degrees 30)
    <| scale 0.7
    <| move (80, 80)
    <| rinclude (scratchEntity, scratchEntityForm) depth
  ]

scratchGlyph = (scratchEntity, scratchEntityForm)



type Scene = { camera: Float, glyphTools: [Glyph], cursor: Glyph }
initialScene = { camera = 0
               , glyphTools = [ scratchGlyph, rectangleGlyph, circleGlyph ]
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


renderGlyph : Glyph -> Form
renderGlyph (entity, entityForm) =
  move entity.pos <| entityForm entity 10

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


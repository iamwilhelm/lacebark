import Window
import Mouse
import Keyboard
import Transform2D

import Vec
import Entity (..)
import Glyph

----- Graphics pipeline
-- Frame coords
-- Glyph coords
-- World coords
-- Camera coords
-- Viewport coords
-- Window coords

center : (Int, Int) -> (Int, Int)
center (w, h) = (div w 2 , div h 2)

relativeMouse : (Int, Int) -> (Int, Int) -> (Int, Int)
relativeMouse (ox, oy) (x, y) = (x - ox, -(y - oy))

toolbarHack: (Int, Int) -> (Int, Int)
toolbarHack (x, y) = (x + 80, y)

type Camera = Entity
initialCamera = initialEntity


-- TODO Scene and tools overlay and cursor overlay should all be inserted in
-- different parts of the graphics transformation pipeline
type Scene = { camera: Camera, glyphTools: [Glyph.Glyph], cursor: Glyph.Glyph }
initialScene = {
    camera = initialCamera
  , glyphTools = [
      Glyph.scratchGlyph
    , Glyph.tentacleGlyph
    , Glyph.rectangleGlyph
    , Glyph.circGlyph
    , Glyph.clubGlyph
    , Glyph.heartGlyph
    , Glyph.diamondGlyph
    ]
  , cursor = Glyph.openPawCursor
  }

updateGlyph : AppInput -> Glyph.Glyph -> Glyph.Glyph
updateGlyph (dt, _, _, _) (entity, entityForm) =
  ({ entity |
     pos <- Vec.add entity.pos <| Vec.mulS entity.vel dt
   },
   entityForm)

updateGlyphTools : AppInput -> [Glyph.Glyph] -> [Glyph.Glyph]
updateGlyphTools appInput glyphTools =
  map (\glyph -> updateGlyph appInput glyph) glyphTools

updateCursor : AppInput -> Glyph.Glyph -> Glyph.Glyph
updateCursor (_, (mouseX, mouseY), mouseDown, _) _ =
  let (entity, entityForm) =
    if mouseDown == True then
      Glyph.closedPawCursor
    else
      Glyph.openPawCursor
  in
    ({ entity | pos <- (toFloat mouseX, toFloat mouseY) }, entityForm)

updateScene : AppInput -> Scene -> Scene
updateScene appInput scene =
  { scene | glyphTools <- updateGlyphTools appInput scene.glyphTools
          , cursor <- updateCursor appInput scene.cursor }


-- graphics transformation pipeline

glyph2World : Glyph.Glyph -> Transform2D.Transform2D
glyph2World (entity, entityForm) =
  Transform2D.multiply
    (Transform2D.translation (fst entity.pos) (snd entity.pos))
    (Transform2D.rotation (degrees entity.rot))

world2Camera: Camera -> Transform2D.Transform2D
world2Camera camera =
  Transform2D.translation (fst camera.pos) (snd camera.pos)

renderScene : Scene -> Form
renderScene scene =
  group <| map (\glyph ->
      groupTransform
        (Transform2D.multiply (world2Camera scene.camera) (glyph2World glyph))
        [Glyph.draw glyph]
    )
    [head scene.glyphTools]

renderToolbar { glyphTools } =
  flow down
  <| map (\glyph -> collage 80 80 [scale 0.2 <| Glyph.draw glyph ]) glyphTools

renderCursor : Glyph.Glyph -> Form
renderCursor (entity, entityForm) =
  move entity.pos <| scale 0.5 <| entityForm entity 10


render : (Int, Int) -> Scene -> Element
render (w, h) scene =
  color lightGray
    <| container w h middle
    <| flow right [
         renderToolbar scene
       , color gray <| collage 1024 600
         [
           renderScene scene
         , renderCursor scene.cursor
         ]
       ]

----- All signals and input into the program

type AppInput = (Time, (Int, Int), Bool, { x: Int, y: Int })

clockInput = lift inSeconds (fps 30)
mouseInput = sampleOn clockInput
               (lift2 relativeMouse
                      (center <~ (toolbarHack <~ Window.dimensions))
                      Mouse.position)
keyInput = Keyboard.wasd
input = (,,,) <~ clockInput ~ mouseInput ~ Mouse.isDown ~ keyInput

main = render <~ Window.dimensions ~ foldp updateScene initialScene input


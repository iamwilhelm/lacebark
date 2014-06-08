import Window
import Mouse
import Keyboard
import Transform2D

import Vec
import Entity (..)
import Glyph

----- Graphics pipeline

center : (Int, Int) -> (Int, Int)
center (w, h) = (div w 2 , div h 2)

relativeMouse : (Int, Int) -> (Int, Int) -> (Int, Int)
relativeMouse (ox, oy) (x, y) = (x - ox, -(y - oy))

-- a hack to correct for width of toolbar
toolbarAdjust: (Int, Int) -> (Int, Int)
toolbarAdjust (x, y) = (x + 80, y)

type Camera = Entity
initialCamera = initialEntity

throughCamera camera forms =
  let
    transform = Transform2D.translation (fst camera.pos) (snd camera.pos)
  in
    groupTransform transform forms 



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


renderGlyph : Glyph.Glyph -> Form
renderGlyph (entity, entityForm) =
  move entity.pos <| entityForm entity 10

renderCursor : Glyph.Glyph -> Form
renderCursor (entity, entityForm) =
  move entity.pos <| scale 0.5 <| entityForm entity 10

renderScene scene =
  [ throughCamera scene.camera [renderGlyph (head scene.glyphTools)]
  , renderCursor scene.cursor
  ]

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

type AppInput = (Time, (Int, Int), Bool, { x: Int, y: Int })

clockInput = lift inSeconds (fps 30)
mouseInput = sampleOn clockInput
               (lift2 relativeMouse
                      (center <~ (toolbarAdjust <~ Window.dimensions))
                      Mouse.position)
keyInput = Keyboard.wasd
input = (,,,) <~ clockInput ~ mouseInput ~ Mouse.isDown ~ keyInput

main = render <~ Window.dimensions ~ foldp updateScene initialScene input


module G where

import Debug

data Statement =
    NoOp
  | Draw Transform Shape

data Transform =
    Move (Float, Float)
  | Rotate Float
  | Scale (Float, Float)
  | Compose Transform Transform
  | NoTF

data Shape =
    Rectangle (Float, Float) Color
  | Circle Float Color

renderTexel : Statement -> Form
renderTexel statement =
  case statement of
    NoOp ->
      -- TODO don't know how to have an empty Glyph. use Maybe?
      filled black <| rect 1 1
    Draw NoTF (Rectangle (x, y) colr) ->
      filled colr <| rect x y
    Draw NoTF (Circle r colr) ->
      filled colr <| circle r
    Draw tform shape ->
      transformTexel tform <| renderTexel (Draw NoTF shape)

transformTexel : Transform -> (Form -> Form)
transformTexel transform =
  case transform of
    Move offset ->
      move offset
    Rotate angle ->
      rotate (degrees angle)
    Compose tf1 tf2 ->
      \x -> transformTexel tf1 <| group [transformTexel tf2 <| x]

--include : Glyph -> Form
--include (entity, entityForm) =
--  -- renderGlyph (entity entityForm)
--  move entity.pos <| entityForm entity 10
--
--rinclude : Glyph -> Int -> Form
--rinclude (entity, entityForm) depth =
--  case depth of
--    0 ->
--      group []
--    _ ->
--      group [ entityForm entity (depth - 1) ]



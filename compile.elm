module Compile where

import Transform2D
import Dict
import Vec
import Glyph

import L (..)
import Term (..)
import Binding
import Bounds

-- drawing functions without coordinate transforms

draw: Binding.Binding -> Glyph.Glyph -> Form
draw binding glyph =
  let
    newBinding = { binding | glyph <- glyph }
  in
    group <| map (\statement -> compile newBinding statement) glyph.statements

drawAsCursor : Binding.Binding -> Glyph.Glyph -> Form
drawAsCursor binding glyph =
  let
    entity = (Binding.glyphOf binding).entity
  in
    move entity.pos <| scale 0.5 <| draw binding glyph

drawToolbar : Binding.Binding -> Glyph.Toolbar -> Form
drawToolbar binding ({ glyphs } as toolbar) =
  move toolbar.entity.pos
  <| toForm
  <| flow left
  <| Dict.foldl (\key glyph cuml ->
       collage 50 50 [
         scale 0.15 <| draw binding glyph
       , Glyph.drawToolbarSelection key toolbar
       ] :: cuml
     ) [] glyphs

drawRubberband : Binding.Binding -> Glyph.Glyph -> Form
drawRubberband binding glyph =
  let
    boundingbox = Bounds.getBoundingBox binding glyph
    size = fst boundingbox
    offset = snd boundingbox
   in
    move offset
    <| group [
         (outlined (dashed black) <| uncurry rect <| size)
       , move (-(fst size) / 2, (snd size) / 2) <| filled black <| circle 5
       , move (0, (snd size) / 2) <| filled black <| circle 5
       , move ((fst size) / 2, (snd size) / 2) <| filled black <| circle 5
       , move (-(fst size) / 2, 0) <| filled black <| circle 5
       , move ((fst size) / 2, 0) <| filled black <| circle 5
       , move (-(fst size) / 2, -(snd size) / 2) <| filled black <| circle 5
       , move (0, -(snd size) / 2) <| filled black <| circle 5
       , move ((fst size) / 2, -(snd size) / 2) <| filled black <| circle 5
       ]



compile : Binding.Binding -> Statement -> Form
compile binding statement =
  case statement of
    NoOp ->
      -- TODO don't know how to have an empty Glyph. use Maybe?
      filled black <| circle 0
    Block statements ->
      group <| map (\statement -> compile binding statement) statements
    Move term statement ->
      group [move (compileTupTerm binding term) <| compile binding statement]
    Rotate term statement ->
      rotate (compileNumTerm binding term) <| compile binding statement
    Scale (Tup x y) statement ->
      groupTransform
        (Transform2D.multiply
          (Transform2D.scaleX (compileNumTerm binding x))
          (Transform2D.scaleY (compileNumTerm binding y))
        )
        [compile binding statement]
    Draw contour ->
      compileContour binding contour
    Map (Proc itername statements) list ->
      let
        iterkey = compileVarName binding itername
        setKey = Binding.setVar (Binding.currentScope binding) iterkey
      in
        group <| concatMap (\n ->
          let
            newBinding = { binding | stack <- [setKey n] }
          in
            map (\statement -> compile newBinding statement) statements
        ) list
    --Include childGlyphName ->
    --  draw toolbar (Dict.getOrFail childGlyphName toolbar.glyphs)

compileContour : Binding.Binding -> Contour -> Form
compileContour binding contour =
  case contour of
    Rectangle (Tup w h) colr ->
      filled colr <| rect (compileNumTerm binding w) (compileNumTerm binding h)
    Rectangle (F r) colr ->
      filled colr <| rect (r * sqrt 2) (r * sqrt 2)
    Rectangle EntityDim colr ->
      filled colr <| uncurry rect (Binding.glyphOf binding).entity.dim
    Circle (F r) colr ->
      filled colr <| circle r
    Triangle (F r) colr ->
      filled colr <| ngon 3 r



module Bounds where

import Vec
import Glyph
import BoundingBox
import L (..)
import Term (..)
import Binding

getBoundingBox : Binding.Binding -> Glyph.Glyph -> BoundingBox.RangedOffset
getBoundingBox binding glyph =
  let
    newBinding = { binding | glyph <- glyph }
  in
    boundsForStatement newBinding (Block glyph.statements)


boundsForStatement : Binding.Binding -> Statement -> BoundingBox.RangedOffset
boundsForStatement binding statement =
  case statement of
    NoOp ->
      ((0, 0), (0, 0))
    Block statements ->
      foldl (\t x -> BoundingBox.merge t x)
        (boundsForStatement binding <| head statements)
        (map (\x -> boundsForStatement binding x) <| tail statements)
    Move term statement ->
      let
        bbox = boundsForStatement binding statement
        size = fst bbox
        offset = snd bbox
      in
        (size, Vec.add offset <| compileTupTerm binding term)
    Scale term statement ->
      let
        bbox = boundsForStatement binding statement
        size = fst bbox
        offset = snd bbox
      in
        (Vec.scale size <| compileTupTerm binding term,
         Vec.scale offset <| compileTupTerm binding term)
    Rotate term statement ->
      boundsForStatement binding statement

    Draw contour ->
      boundsForContour binding contour

boundsForContour : Binding.Binding -> Contour -> (Vec.Vec, Vec.Vec)
boundsForContour binding contour =
  case contour of
    Rectangle (Tup w h) colr ->
      ((compileNumTerm binding w, compileNumTerm binding h), (0, 0))
    Rectangle EntityDim colr ->
      ((Binding.glyphOf binding).entity.dim, (0, 0))
    Circle r colr ->
      let
        radius = 2 * compileNumTerm binding r
      in
        ((radius, radius), (0, 0))
    Triangle r colr ->
      let
        radius = 2 * compileNumTerm binding r
      in
        ((radius, radius), (0, 0))



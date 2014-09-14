module Term where

import Vec
import L (..)
import Binding

compileNumTerm : Binding.Binding -> Term -> Float
compileNumTerm binding term =
  case term of
    Degrees r ->
      degrees r
    F r ->
      r
    N ->
      Binding.getVar (Binding.currentScope binding) "n"
    M ->
      Binding.getVar (Binding.currentScope binding) "m"
    Add term1 term2 ->
      compileNumTerm binding term1 + compileNumTerm binding term2
    Sub term1 term2 ->
      compileNumTerm binding term1 - compileNumTerm binding term2
    Mul term1 term2 ->
      compileNumTerm binding term1 * compileNumTerm binding term2
    Div term1 term2 ->
      compileNumTerm binding term1 / compileNumTerm binding term2

compileVarName binding varname =
  case varname of
    N -> "n"
    M -> "m"

compileTupTerm : Binding.Binding -> Term -> (Float, Float)
compileTupTerm binding term =
  case term of
    EntityPos ->
      (Binding.glyphOf binding).entity.pos
    EntityOffset x y ->
      Vec.add (Binding.glyphOf binding).entity.pos (x, y)
    Tup x y ->
      (compileNumTerm binding x, compileNumTerm binding y)



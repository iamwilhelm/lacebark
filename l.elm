module L where

-- BNF for language

data Statement =
    NoOp
  | Block [Statement]
  | Proc Term [Statement]
  | Move Term Statement
  | Rotate Term Statement
  | Scale Term Statement
  | Draw Contour
  | Map Statement [Float]
  | Include String

data Contour =
    Rectangle Term Color
  | Circle Term Color
  | Triangle Term Color

data Term =
    Tup Term Term
  | F Float
  | Degrees Float
  | Ref Int Int
  | EntityOffset Float Float
  | EntityPos
  | EntityDim
  | Add Term Term
  | Sub Term Term
  | Mul Term Term
  | Div Term Term
  | N
  | M


module Vec where

type Vec = (Float, Float)

add : Vec -> Vec -> Vec
add (ax, ay) (bx, by) = (ax + bx, ay + by)

mulS : Vec -> Float -> Vec
mulS (x, y) a = (x * a, y * a)


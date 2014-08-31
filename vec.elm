module Vec where

type Vec = (Float, Float)

add : Vec -> Vec -> Vec
add (ax, ay) (bx, by) = (ax + bx, ay + by)

sub : Vec -> Vec -> Vec
sub (ax, ay) (bx, by) = (ax - bx, ay - by)

dot : Vec -> Vec -> Float
dot (ax, ay) (bx, by) = ax * bx + ay * by

dist : Vec -> Float
dist (x, y) = sqrt <| x * x + y * y

mulS : Vec -> Float -> Vec
mulS (x, y) a = (x * a, y * a)

scale : Vec -> Vec -> Vec
scale (x, y) (sx, sy) = (x * sx, y * sy)

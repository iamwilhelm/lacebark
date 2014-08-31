module BoundingBox where

import Vec

type RangedOffset = (Vec.Vec, Vec.Vec)

mergeIntervals : RangedOffset -> Float
mergeIntervals ((interval1, interval2), (pos1, pos2)) =
  let
    left_pos = min (pos1 - interval1 / 2) (pos2 - interval2 / 2)
    right_pos = max (pos1 + interval1 / 2) (pos2 + interval2 / 2)
  in
    abs (right_pos - left_pos)

mergeOffsets : RangedOffset -> Float
mergeOffsets ((interval1, interval2), (pos1, pos2)) =
  let
    left_pos = min (pos1 - interval1 / 2) (pos2 - interval2 / 2)
    right_pos = max (pos1 + interval1 / 2) (pos2 + interval2 / 2)
  in
    (left_pos + right_pos) / 2

merge : RangedOffset -> RangedOffset -> RangedOffset
merge (size1, offset1) (size2, offset2) =
  let
    sizeX = mergeIntervals ((fst size1, fst size2), (fst offset1, fst offset2))
    sizeY = mergeIntervals ((snd size1, snd size2), (snd offset1, snd offset2))
    offsetX = mergeOffsets ((fst size1, fst size2), (fst offset1, fst offset2))
    offsetY = mergeOffsets ((snd size1, snd size2), (snd offset1, snd offset2))
  in
    ((sizeX, sizeY), (offsetX, offsetY))


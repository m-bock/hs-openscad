module Tmp where

type DrillSize = Double
type Pos = Double
type Length = Double

f :: DrillSize -> Pos
f i = sum (fmap g [0, 0.5 .. (i - 0.5)]) + (g i * 0.5)

g :: DrillSize -> Length
g i = i * 2
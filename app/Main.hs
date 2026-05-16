{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main where

import OpenSCAD.Model (Model(..), Facet(..), V3, render)


pitch :: Double
pitch = 9

baseHeight :: Double
baseHeight = 2.5

width :: Double
width = 17

diameter :: Double
diameter = 3

clearance :: Double
clearance = 0.2

s1 :: Model
s1 = Union
  [ Translate (- (width / 2), 0, 0) $ Cube { size = (width, pitch, baseHeight) }
  , Translate (0, pitch - r, 0)
     $ Cylinder { height = 10, radius1 = r, radius2 = r, facet = NumFacets 30 }
  ]
  where
    r = (diameter / 2) - clearance

s2 :: Model
s2 = Union $ fmap (\i -> Translate (0, i * pitch, 0) s1) [0..5]



sketch :: Model
sketch = Union
 [ Translate (-40, 0, 0) s2
 , Translate (40, 0, 0) s2
 , Translate (0, 0, 0) s2
 ]

main :: IO ()
main = writeFile "/home/m/Desktop/scad/lustre.scad" (render sketch)

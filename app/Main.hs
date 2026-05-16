{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where

import Raw (Ast(..))
import qualified Raw

data Shape = Rectangle Double Double
             -- | Circle Double Facet
             -- | Polygon Int [Vector2d] [[Int]]
             -- | Projection Bool Model3d
             -- | Offset Double Join Shape
           deriving Show

data Solid -- Sphere Double --Facet
           = Cube { size :: (Double, Double, Double) }
           | Cylinder { height :: Double, radius1 :: Double, radius2 :: Double } -- Facet
        --    | ObCylinder Double Double Double Facet
        --    | Polyhedron Int [Vector3d] Sides
        --    | MultMatrix TransMatrix Model3d
        --    | LinearExtrude Double Double Vector2d Int Int Facet Model2d
        --    | RotateExtrude Int Facet Model2d
        --    | Surface FilePath Bool Int
        --    | ToSolid Model2d
           deriving Show



data Model v = Solid Solid
            --  | Shape Shape
            --  | Scale v (Model v)
            --  | Resize v (Model v)
            --  | Rotate v (Model v)
            | Translate v (Model v)
            --  | Mirror v (Model v)
            --  | Color (Colour Double) (Model v)
            --  | Transparent (AlphaColour Double) (Model v)
            --  -- and combinations
            | Union [Model v]
            --  | Intersection [Model v]
            --  | Minkowski [Model v]
            --  | Hull [Model v]
            --  | Difference (Model v) (Model v)
            --  -- And oddball stuff control
            --  | Import FilePath
            --  | Var Facet [Model v]
            --  deriving Show

class Vector v
  where
    toVec :: v -> [Double]

instance Vector (Double, Double, Double) where
  toVec (x, y, z) = [x, y, z]

instance Vector (Double, Double) where
  toVec (x, y) = [x, y]

renderVec :: Vector v => v -> Raw.Ast
renderVec v = Vec (fmap LitDouble (toVec v))

render :: Vector v => Model v -> Raw.Ast
render = \case
  --Shape2d shape2d -> rShape2d shape2d
  Solid solid       -> rSolid solid
  Union models      -> App "union" [] (fmap render models)
  Translate v model -> App "translate" [(Just "v", renderVec v)] [render model]
  where
    rSolid :: Solid -> Raw.Ast
    rSolid = \case
      Cube { size = (x, y, z) } -> App "cube"
                              [(Just "size", Vec [LitDouble x, LitDouble y, LitDouble z])]
                              []
      Cylinder h r1 r2 -> App "cylinder"
                              [(Just "h", LitDouble h), (Just "r1", LitDouble r1), (Just "r2", LitDouble r2)]
                              []

xx :: Model (Double, Double, Double)
xx = Union $ fmap (\i -> Translate (0, i * 90, 0) (Solid (Cylinder 70 25 25))) [0..4]

sketch :: Model (Double, Double, Double)
sketch = Union [xx, Solid (Cube { size = (170, 700, 10) })]

main :: IO ()
main = writeFile "tmp/out.scad" (Raw.render (render sketch))

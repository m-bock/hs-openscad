{-# LANGUAGE LambdaCase #-}
module Main where

import Raw

data Shape = Rectangle Double Double
             -- | Circle Double Facet
             -- | Polygon Int [Vector2d] [[Int]]
             -- | Projection Bool Model3d
             -- | Offset Double Join Shape
           deriving Show

data Solid -- Sphere Double --Facet
           = Box Double Double Double
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
            --  | Solid Solid
            --  | Scale v (Model v)
            --  | Resize v (Model v)
            --  | Rotate v (Model v)
            --  | Translate v (Model v)
            --  | Mirror v (Model v)
            --  | Color (Colour Double) (Model v)
            --  | Transparent (AlphaColour Double) (Model v)
            --  -- and combinations
            --  | Union [Model v]
            --  | Intersection [Model v]
            --  | Minkowski [Model v]
            --  | Hull [Model v]
            --  | Difference (Model v) (Model v)
            --  -- And oddball stuff control
            --  | Import FilePath
            --  | Var Facet [Model v]
            --  deriving Show

class Vector v

render :: Vector v => Model v -> Ast
render = \case
  --Shape2d shape2d -> rShape2d shape2d
  Solid solid -> rSolid solid
  where
    rSolid :: Solid -> Ast
    rSolid = \case
      Box x y z -> Vec3 (LitDouble x) (LitDouble y) (LitDouble z)

main :: IO ()
main = putStrLn "Hello, Haskell!"

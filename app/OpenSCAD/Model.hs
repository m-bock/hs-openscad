{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
module OpenSCAD.Model where

import OpenSCAD.Raw (Ast(..))
import qualified OpenSCAD.Raw as Raw

data Facet = MinAngle Double | MinSize Double | NumFacets Int | Def
  deriving Show

data Shape = Rectangle Double Double
             -- | Circle Double Facet
             -- | Polygon Int [Vector2d] [[Int]]
             -- | Projection Bool Model3d
             -- | Offset Double Join Shape
           deriving Show

data Solid -- Sphere Double --Facet
           = Cube { size :: (Double, Double, Double) }
           | Cylinder { height :: Double, radius1 :: Double, radius2 :: Double, facet :: Facet }
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

toRawVec :: Vector v => v -> Raw.Ast
toRawVec v = Vec (fmap LitDouble (toVec v))

toRaw :: Vector v => Model v -> Raw.Ast
toRaw = \case
  --Shape2d shape2d -> rShape2d shape2d
  Solid solid       -> rSolid solid
  Union models      -> App "union" [] (fmap toRaw models)
  Translate v model -> App "translate" [(Just "v", toRawVec v)] [toRaw model]
  where
    rSolid :: Solid -> Raw.Ast
    rSolid = \case
      Cube { size = (x, y, z) }
        -> App
             "cube"
             [(Just "size", Vec [LitDouble x, LitDouble y, LitDouble z])]
             []
      Cylinder { height, radius1, radius2, facet }
        -> App
             "cylinder"
             ([(Just "h", LitDouble height), (Just "r1", LitDouble radius1), (Just "r2", LitDouble radius2)] ++ renderFacet facet)
             []

render :: Vector v => Model v -> String
render = Raw.render . toRaw

renderFacet :: Facet -> [ (Maybe String, Raw.Ast) ]
renderFacet = \case
  MinAngle angle -> [ (Just "$fa", LitDouble angle) ]
  MinSize size   -> [ (Just "$fs", LitDouble size) ]
  NumFacets num  -> [ (Just "$fn", LitInt num) ]
  Def            -> []

type V3 = (Double, Double, Double)

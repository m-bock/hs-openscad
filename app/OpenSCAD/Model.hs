module OpenSCAD.Model (Model(..), Facet(..), V3, render) where

import OpenSCAD.Raw (Ast(..))
import qualified OpenSCAD.Raw as Raw

data Facet = MinAngle Double | MinSize Double | NumFacets Int | Def
  deriving Show

-- data Shape = Rectangle Double Double
--              -- | Circle Double Facet
--              -- | Polygon Int [Vector2d] [[Int]]
--              -- | Projection Bool Model3d
--              -- | Offset Double Join Shape
--            deriving Show

-- data Solid -- Sphere Double --Facet
--            = Cube { size :: (Double, Double, Double) }
--            | Cylinder { height :: Double, radius1 :: Double, radius2 :: Double, facet :: Facet }
--         --    | ObCylinder Double Double Double Facet
--         --    | Polyhedron Int [Vector3d] Sides
--         --    | MultMatrix TransMatrix Model3d
--         --    | LinearExtrude Double Double Vector2d Int Int Facet Model2d
--         --    | RotateExtrude Int Facet Model2d
--         --    | Surface FilePath Bool Int
--         --    | ToSolid Model2d
--            deriving Show



data Model  = Cube { size :: (Double, Double, Double) }
            | Cylinder { height :: Double, radius1 :: Double, radius2 :: Double, facet :: Facet }
            --  | Shape Shape
            --  | Scale v (Model v)
            --  | Resize v (Model v)
            --  | Rotate v (Model v)
            | Translate V3 Model
            --  | Mirror v (Model v)
            --  | Color (Colour Double) (Model v)
            --  | Transparent (AlphaColour Double) (Model v)
            --  -- and combinations
            | Union [Model]
            --  | Intersection [Model v]
            --  | Minkowski [Model v]
            --  | Hull [Model v]
            --  | Difference (Model v) (Model v)
            --  -- And oddball stuff control
            --  | Import FilePath
            --  | Var Facet [Model v]
            --  deriving Show

toRawVec :: V3 -> Raw.Ast
toRawVec (x, y, z) = Vec [LitDouble x, LitDouble y, LitDouble z]

toRaw :: Model -> Raw.Ast
toRaw = \case
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
  Union models      -> App "union" [] (fmap toRaw models)
  Translate v model -> App "translate" [(Just "v", toRawVec v)] [toRaw model]

render :: Model -> String
render = Raw.render . toRaw

renderFacet :: Facet -> [ (Maybe String, Raw.Ast) ]
renderFacet = \case
  MinAngle angle -> [ (Just "$fa", LitDouble angle) ]
  MinSize size   -> [ (Just "$fs", LitDouble size) ]
  NumFacets num  -> [ (Just "$fn", LitInt num) ]
  Def            -> []

type V3 = (Double, Double, Double)

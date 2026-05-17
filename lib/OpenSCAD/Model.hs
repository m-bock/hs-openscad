{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use :" #-}

module OpenSCAD.Model (Model3D(..), Model2D(..), Facets(..), V3, V2, RGB, render) where

import OpenSCAD.Raw (Ast(..))
import qualified OpenSCAD.Raw as Raw

data Facets = Facets {
  fa :: Maybe Double,
  fs :: Maybe Double,
  fn :: Maybe Int
}
  deriving Show

type V3 a = (a, a, a)
type V2 a = (a, a)
type RGB = V3 Double

-------------------------------------------------------------------------------
--- 2D
-------------------------------------------------------------------------------

data Model2D
  = Primitive2D Primitive2D
  | Transform2D Transform2D  [Model2D]
  | Pojection2D Projection2D [Model3D]

data Projection2D = Projection2D { cut :: Maybe Bool }

data Primitive2D
  = Circle2D  { d :: Double, _facets :: Maybe Facets }
  | Square2D  { size :: V2 Double, center :: Maybe Bool }
  | Polygon2D { points :: [V2 Double], paths :: Maybe [[Int]], convexity :: Maybe Int }

data Transform2D
  = Scale2D        { v :: V2 Double }
  | Resize2D       { v :: V2 Double, auto :: Maybe (V2 Bool) } -- !
  | RotateEuler2D  { v :: V2 Double }
  | RotateAxis2D   { a :: Double }
  | Translate2D    { v :: V2 Double }
  | Mirror2D       { v :: V2 Double }
  | Color2D        { c :: RGB, alpha :: Maybe Double }
  | OffsetRadial2D { r :: Double }
  | OffsetDelta2D  { delta :: Double, chamfer :: Maybe Bool }
  | Fill2D
  | Minkowski2D
  | Hull2D
  | Union2D
  | Intersection2D
  | Difference2D

-------------------------------------------------------------------------------
--- 3D
-------------------------------------------------------------------------------

data Model3D
  = Primitive3D Primitive3D
  | Transform3D Transform3D [Model3D]
  | Extrude3D   Extrude3D   [Model2D]

data Extrude3D
  = LinearExtrude
      { height    :: Double
      , centerP    :: Maybe Bool
      , twist     :: Maybe Double
      , scale     :: Maybe Double
      , slices    :: Maybe Int
      , segments  :: Maybe Int
      , convexity :: Maybe Int
      }
  | RotateExtrude
      { angle :: Double
      , start :: Double
      , convexity :: Maybe Int
      , _facets :: Maybe Facets
      }

data Primitive3D
  = Cube3D       { size :: V3 Double }
  | Cylinder3D   { h :: Double, d1 :: Double, d2 :: Double, _facets :: Maybe Facets }
  | Sphere3D     { d :: Double, _facets :: Maybe Facets }
  | Polyhedron3D { points :: [V3 Double], faces :: Maybe [[Int]], convexity :: Maybe Int }

data Transform3D
  = Scale3D        { v :: V3 Double }
  | Resize3D       { v :: V3 Double, auto :: Maybe (V3 Bool) }
  | RotateEuler3D  { v :: V3 Double }
  | RotateAxis3D   { a :: Double, v :: V3 Double }
  | Translate3D    { v :: V3 Double }
  | Mirror3D       { v :: V3 Double }
  | Color3D        { c :: Maybe RGB, alpha :: Maybe Double }
  | Union3D  
  | Intersection3D 
  | Difference3D
  | Minkowski3D
  | Hull3D
  
-------------------------------------------------------------------------------
--- ToRaw
-------------------------------------------------------------------------------

toRawModel2D :: Model2D -> Raw.Ast
toRawModel2D = \case
  Primitive2D (Circle2D { d, _facets })
    -> App "circle"
         ([(Just "d", LitDouble d)] ++ toRawFacets _facets)
         []
  Primitive2D (Square2D { size, center })
    -> App "square"
         ([(Just "size", toRawVec2Double size)] ++ maybe [] (\c -> [(Just "center", LitBool c)]) center)
         []


--   Circle2D Circle2DSpec { d }
--     -> App "circle"
--          [(Just "d", LitDouble d)]
--          []
--   Rect2D (x, y)
--     -> App "square"
--          [(Nothing, Vec [LitDouble x, LitDouble y])]
--          []

--   Scale2D (x, y) model
--     -> App "scale"
--          [(Just "v", toRawVec2Double (x, y))]
--          [toRawModel2D model]
--   Resize2D v model
--     -> App "resize"
--          (toRawVec2MaybeDouble v)
--          [toRawModel2D model]
--   Rotate2D angle model
--     -> App "rotate"
--          [(Just "angle", LitDouble angle)]
--          [toRawModel2D model]
--   Translate2D v model
--     -> App "translate"
--          [(Just "v", toRawVec2Double v)]
--          [toRawModel2D model]
--   Mirror2D v model
--     -> App "mirror"
--          [(Just "v", toRawVec2Double v)]
--          [toRawModel2D model]

--   Color2D ColorSpec { c, alpha } model
--     -> App "color"
--          [(Just "c", toRawVec3Double c), (Just "alpha", LitDouble alpha)]
--          [toRawModel2D model]

-- toRawModel3D :: Model3D -> Raw.Ast
-- toRawModel3D = \case
--   Box3D (x, y, z)
--     -> App "cube"
--          [(Nothing, Vec [LitDouble x, LitDouble y, LitDouble z])]
--          []
--   Cone3D ConeSpec { h, d1, d2, facet }
--     -> App "cylinder"
--          ([(Just "h", LitDouble h), (Just "d1", LitDouble d1), (Just "d2", LitDouble d2)] 
--           ++ toRawFacet facet)
--          []
--   Sphere3D SphereSpec { d }
--     -> App "sphere"
--          [(Just "d", LitDouble d)]
--          []

--   Scale3D v model
--     -> App "scale"
--          [(Just "v", toRawVec3Double v)]
--          [toRawModel3D model]
--   Resize3D v model
--     -> App "resize"
--          (toRawVec3MaybeDouble v)
--          [toRawModel3D model]
--   RotateEuler3D v model
--     -> App "rotate_euler"
--          [(Just "v", toRawVec3Double v)]
--          [toRawModel3D model]
--   RotateAxis3D angle v model
--     -> App "rotate_axis"
--          [(Just "angle", LitDouble angle), (Just "v", toRawVec3Double v)]
--          [toRawModel3D model]
--   Translate3D v model
--     -> App "translate"
--          [(Just "v", toRawVec3Double v)]
--          [toRawModel3D model]
--   Mirror3D v model
--     -> App "mirror"
--          [(Just "v", toRawVec3Double v)]
--          [toRawModel3D model]

--   Color3D ColorSpec { c, alpha } model
--     -> App "color"
--          [(Just "c", toRawVec3Double c), (Just "alpha", LitDouble alpha)]
--          [toRawModel3D model]

--   Union3D models      
--     -> App "union"
--          []
--          (fmap toRawModel3D models)
--   Intersection3D models
--     -> App "intersection"
--          []
--          (fmap toRawModel3D models)
--   Difference3D model models
--     -> App "difference"
--          []
--          (toRawModel3D model : fmap toRawModel3D models)

--   Minkowski3D models
--     -> App "minkowski"
--          []
--          (fmap toRawModel3D models)
--   Hull3D models
--     -> App "hull"
--          []
--          (fmap toRawModel3D models)
  

toRawFacets :: Maybe Facets -> [ (Maybe String, Raw.Ast) ]
toRawFacets = \case
  Just Facets { fa, fs, fn } ->
    (maybe [] (\a -> [(Just "$fa", LitDouble a)]) fa) ++
    (maybe [] (\s -> [(Just "$fs", LitDouble s)]) fs) ++
    (maybe [] (\n -> [(Just "$fn", LitInt n)   ]) fn)
  Nothing ->
    []


-- toRawVec3Double :: V3 Double -> Raw.Ast
-- toRawVec3Double (x, y, z) = Vec [LitDouble x, LitDouble y, LitDouble z]

toRawVec2Double :: V2 Double -> Raw.Ast
toRawVec2Double (x, y) = Vec [LitDouble x, LitDouble y]

-- toRawVec2MaybeDouble :: V2 (Maybe Double) -> [ (Maybe String, Raw.Ast) ]
-- toRawVec2MaybeDouble (x, y) =
--   [ ( Nothing
--     , Vec [ LitDouble (fromMaybe 0 x), LitDouble (fromMaybe 0 y) ]
--     )
--   , ( Just "auto"
--     , LitBool True
--     )
--   ]

-- toRawVec3MaybeDouble :: V3 (Maybe Double) -> [ (Maybe String, Raw.Ast) ]
-- toRawVec3MaybeDouble (x, y, z) =
--   [ ( Nothing
--     , Vec [ LitDouble (fromMaybe 0 x), LitDouble (fromMaybe 0 y), LitDouble (fromMaybe 0 z) ]
--     )
--   , ( Just "auto"
--     , LitBool True
--     )
--   ]

-- -------------------------------------------------------------------------------
-- --- Render
-- -------------------------------------------------------------------------------

-- render :: Model3D -> String
-- render = Raw.render . toRawModel3D

render :: Model3D -> String
render = undefined
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use :" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# HLINT ignore "Use ++" #-}
{-# HLINT ignore "Evaluate" #-}

module OpenSCAD.Model (Model3D(..), Model2D(..), Facets(..), V3, V2, RGB, render) where

import OpenSCAD.Raw (Ast(..))
import qualified OpenSCAD.Raw as Raw
import Data.Maybe (catMaybes)

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

optional :: (a -> b) -> Maybe a -> [b]
optional f = maybe [] (pure . f)

optionals :: (a -> [b]) -> Maybe a -> [b]
optionals f = maybe [] f

required :: a -> [a]
required x = [x]

toRawModel2D :: Model2D -> Raw.Ast
toRawModel2D = \case
  Primitive2D (Circle2D { d, _facets })
    -> App "circle"
         (concat
           [ required (Just "d", LitDouble d)
           , optionals toRawFacets _facets
           ]
         )
         []
  Primitive2D (Square2D { size, center })
    -> App "square"
         (concat
           [ required (Just "size", toRawVec2Double size)
           , optional (\c -> (Just "center", LitBool c)) center
           ]
         )
         []
  Primitive2D (Polygon2D { points, paths, convexity })
    -> App "polygon"
         (concat
           [ required (Just "points", Vec $ map toRawVec2Double points)
           , optional (\p -> (Just "paths", Vec $ map (Vec . map LitInt) p)) paths
           , optional (\c -> (Just "convexity", LitInt c)) convexity
           ]
         )
         []
  Transform2D (Scale2D { v }) children
    -> App "scale"
         (concat
           [ required (Just "v", toRawVec2Double v)
           ]
         )
         (map toRawModel2D children)
  Transform2D (Resize2D { v, auto }) children
    -> App "resize"
         (concat
           [ required (Just "v", toRawVec2Double v)
           , optional (\(a1, a2) -> (Just "auto", Vec [LitBool a1, LitBool a2])) auto
           ]
         )
         (map toRawModel2D children)
  Transform2D (RotateEuler2D { v }) children
    -> App "rotate_euler"
         (concat
           [ required (Just "v", toRawVec2Double v)
           ]
         )
         (map toRawModel2D children)
  Transform2D (RotateAxis2D { a }) children
    -> App "rotate_axis"
         (concat
           [ required (Just "a", LitDouble a)
           ]
         )
         (map toRawModel2D children)
  Transform2D (Translate2D { v }) children
    -> App "translate"
         (concat
           [ required (Just "v", toRawVec2Double v)
           ]
         )
         (map toRawModel2D children)
  Transform2D (Mirror2D { v }) children
    -> App "mirror"
         (concat
           [ required (Just "v", toRawVec2Double v)
           ]
         )
         (map toRawModel2D children)
  Transform2D (Color2D { c, alpha }) children
    -> App "color"
         (concat
           [ required (Just "c", toRawVec3Double c)
           , optional (\a -> (Just "alpha", LitDouble a)) alpha
           ]
         )
         (map toRawModel2D children)
  Transform2D (OffsetRadial2D { r }) children
    -> App "offset_radial"
         (concat
           [ required (Just "r", LitDouble r)
           ]
         )
         (map toRawModel2D children)
  Transform2D (OffsetDelta2D { delta, chamfer }) children
    -> App "offset_delta"
         (concat
           [ required (Just "delta", LitDouble delta)
           , optional (\c -> (Just "chamfer", LitBool c)) chamfer
           ]
         )
         (map toRawModel2D children)
  Transform2D (Fill2D) children
    -> App "fill"
         []
         (map toRawModel2D children)
  Transform2D (Minkowski2D) children
    -> App "minkowski"
         []
         (map toRawModel2D children)
  Transform2D (Hull2D) children
    -> App "hull"
         []
         (map toRawModel2D children)
  Transform2D (Union2D) children
    -> App "union"
         []
         (map toRawModel2D children)
  Transform2D (Intersection2D) children
    -> App "intersection"
         []
         (map toRawModel2D children)
  Transform2D (Difference2D) children
    -> App "difference"
         []
         (map toRawModel2D children)


toRawModel3D :: Model3D -> Raw.Ast
toRawModel3D = \case
  Primitive3D (Cube3D { size })
    -> App "cube"
         (concat
           [ required (Just "size", toRawVec3Double size)
           ]
         )
         []
  Primitive3D (Cylinder3D { h, d1, d2, _facets })
    -> App "cylinder"
         (concat
           [ required (Just "h", LitDouble h)
           , required (Just "d1", LitDouble d1)
           , required (Just "d2", LitDouble d2)
           , optionals toRawFacets _facets
           ]
         )
         []
  Primitive3D (Sphere3D { d, _facets })
    -> App "sphere"
         (concat
           [ required (Just "d", LitDouble d)
           , optionals toRawFacets _facets
           ]
         )
         []
  Primitive3D (Polyhedron3D { points, faces, convexity })
    -> App "polyhedron"
         (concat
           [ required (Just "points", Vec $ map toRawVec3Double points)
           , optional (\f -> (Just "faces", Vec $ map (Vec . map LitInt) f)) faces
           , optional (\c -> (Just "convexity", LitInt c)) convexity
           ]
         )
         []
  Transform3D (Scale3D { v }) children
    -> App "scale"
         (concat
           [ required (Just "v", toRawVec3Double v)
           ]
         )
         (map toRawModel3D children)
  Transform3D (Resize3D { v, auto }) children
    -> App "resize"
         (concat
           [ required (Just "v", toRawVec3Double v)
           , optional (\(a1, a2, a3) -> (Just "auto", Vec [LitBool a1, LitBool a2, LitBool a3])) auto
           ]
         )
         (map toRawModel3D children)
  Transform3D (RotateEuler3D { v }) children
    -> App "rotate"
         (concat
           [ required (Just "v", toRawVec3Double v)
           ]
         )
         (map toRawModel3D children)
  Transform3D (RotateAxis3D { a, v }) children
    -> App "rotate"
         (concat
           [ required (Just "a", LitDouble a)
           , required (Just "v", toRawVec3Double v)
           ]
         )
         (map toRawModel3D children)
  Transform3D (Translate3D { v }) children
    -> App "translate"
         (concat
           [ required (Just "v", toRawVec3Double v)
           ]
         )
         (map toRawModel3D children)
  Transform3D (Mirror3D { v }) children
    -> App "mirror"
         (concat
           [ required (Just "v", toRawVec3Double v)
           ]
         )
         (map toRawModel3D children)
  Transform3D (Color3D { c, alpha }) children
    -> App "color"
         (concat
           [ optional (\co -> (Just "c", toRawVec3Double co)) c
           , optional (\a -> (Just "alpha", LitDouble a)) alpha
           ]
         )
         (map toRawModel3D children)
  Transform3D (Union3D) children
    -> App "union"
         []
         (map toRawModel3D children)
  Transform3D (Intersection3D) children
    -> App "intersection"
         []
         (map toRawModel3D children)
  Transform3D (Difference3D) children
    -> App "difference"
         []
         (map toRawModel3D children)
  Transform3D (Minkowski3D) children
    -> App "minkowski"
         []
         (map toRawModel3D children)
  Transform3D (Hull3D) children
    -> App "hull"
         []
         (map toRawModel3D children)

toRawFacets :: Facets -> [ (Maybe String, Raw.Ast) ]
toRawFacets Facets { fa, fs, fn } =
    (maybe [] (\a -> [(Just "$fa", LitDouble a)]) fa) ++
    (maybe [] (\s -> [(Just "$fs", LitDouble s)]) fs) ++
    (maybe [] (\n -> [(Just "$fn", LitInt n)   ]) fn)


toRawVec3Double :: V3 Double -> Raw.Ast
toRawVec3Double (x, y, z) = Vec [LitDouble x, LitDouble y, LitDouble z]

toRawVec2Double :: V2 Double -> Raw.Ast
toRawVec2Double (x, y) = Vec [LitDouble x, LitDouble y]


-------------------------------------------------------------------------------
--- Render
-------------------------------------------------------------------------------

render :: Model3D -> String
render = Raw.render . toRawModel3D

{- FOURMOLU_DISABLE -}

module OpenSCAD.Model
  ( Model3D(..)
  , Model2D(..)
  , Primitive2D(..), Transform2D(..), Projection2D(..)
  , Primitive3D(..), Transform3D(..), Extrude3D(..)
  , Facets(..)
  , V3
  , V2
  , RGB
  , render3D
  , render2D
  ) where

import OpenSCAD.Raw (Ast(..), Lit(..))
import qualified OpenSCAD.Raw as Raw
import Data.Monoid (First(..))

-------------------------------------------------------------------------------
--- Types
-------------------------------------------------------------------------------

data Facets = Facets {
  fa :: Maybe Double,
  fs :: Maybe Double,
  fn :: Maybe Int
}
  deriving Show

instance Semigroup Facets where
  (Facets fa fs fn) <> (Facets fa' fs' fn') = Facets {
    fa = getFirst (First fa <> First fa'),
    fs = getFirst (First fs <> First fs'),
    fn = getFirst (First fn <> First fn')
  }

instance Monoid Facets where
  mempty = Facets Nothing Nothing Nothing

type V3 a = (a, a, a)
type V2 a = (a, a)
type RGB = V3 Double

type Comment = String

-------------------------------------------------------------------------------
--- 2D
-------------------------------------------------------------------------------

data Model2D
  = Primitive2D  Primitive2D
  | Transform2D  Transform2D  [Model2D]
  | Projection2D Projection2D [Model3D]
  | Comment2D Comment Model2D

data Projection2D = RegularProjection2D { cut :: Maybe Bool }

data Primitive2D
  = Circle2D  { d :: Double, _facets :: Maybe Facets }
  | Square2D  { size :: V2 Double, center :: Maybe Bool }
  | Polygon2D { points :: [V2 Double], paths :: Maybe [[Int]], convexity :: Maybe Int }

data Transform2D
  = Scale2D        { v2 :: V2 Double }
  | Resize2D       { newSize :: V2 Double, auto :: Maybe (V2 Bool) }
  | RotateEuler2D  { v2 :: V2 Double }
  | RotateAxis2D   { a :: Double, mv2 :: Maybe (V2 Double) }
  | Translate2D    { v3 :: V3 Double } -- sic! 2d shapes can be translated in 3d space
  | Mirror2D       { v2 :: V2 Double }
  | Color2D        { c :: Maybe RGB, alpha :: Maybe Double }
  | OffsetRadial2D { r :: Double, _facets :: Maybe Facets }
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
  | Comment3D Comment Model3D

data Extrude3D
  = LinearExtrude
      { height    :: Double
      , center    :: Maybe Bool
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
  = Cube3D
      { size :: V3 Double
      }
  | Cylinder3D
      { h :: Double
      , d1 :: Double
      , d2 :: Double
      , _facets :: Maybe Facets
      }
  | Sphere3D
      { d :: Double
      , _facets :: Maybe Facets
      }
  | Polyhedron3D
      { points :: [V3 Double]
      , faces :: Maybe [[Int]]
      , convexity :: Maybe Int
      }

data Transform3D
  = Scale3D        { v :: V3 Double }
  | Resize3D       { newSize :: V3 Double, auto :: Maybe (V3 Bool) }
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
  Comment2D comment ast -> Comment comment (toRawModel2D ast)
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
           [ required (Just "points", LitVec $ map toRawVec2Double points)
           , optional (\p -> (Just "paths", LitVec $ map (LitVec . map LitInt) p)) paths
           , optional (\c -> (Just "convexity", LitInt c)) convexity
           ]
         )
         []
  Transform2D (Scale2D { v2 }) children
    -> App "scale"
         (concat
           [ required (Just "v", toRawVec2Double v2)
           ]
         )
         (map toRawModel2D children)
  Transform2D (Resize2D { newSize, auto }) children
    -> App "resize"
         (concat
           [ required (Just "newsize", toRawVec2Double newSize)
           , optional (\(a1, a2) -> (Just "auto", LitVec [LitBool a1, LitBool a2])) auto
           ]
         )
         (map toRawModel2D children)
  Transform2D (RotateEuler2D { v2 }) children
    -> App "rotate"
         (concat
           [ required (Just "v", toRawVec2Double v2)
           ]
         )
         (map toRawModel2D children)
  Transform2D (RotateAxis2D { a, mv2 }) children
    -> App "rotate"
         (concat
           [ required (Just "a", LitDouble a)
           , optional (\v -> (Just "v", toRawVec2Double v)) mv2
           ]
         )
         (map toRawModel2D children)
  Transform2D (Translate2D { v3 }) children
    -> App "translate"
         (concat
           [ required (Just "v", toRawVec3Double v3)
           ]
         )
         (map toRawModel2D children)
  Transform2D (Mirror2D { v2 }) children
    -> App "mirror"
         (concat
           [ required (Just "v", toRawVec2Double v2)
           ]
         )
         (map toRawModel2D children)
  Transform2D (Color2D { c, alpha }) children
    -> App "color"
         (concat
           [ optional (\co -> (Just "c", toRawVec3Double co)) c
           , optional (\a -> (Just "alpha", LitDouble a)) alpha
           ]
         )
         (map toRawModel2D children)
  Transform2D (OffsetRadial2D { r, _facets }) children
    -> App "offset"
         (concat
           [ required (Just "r", LitDouble r)
           , optionals toRawFacets _facets
           ]
         )
         (map toRawModel2D children)
  Transform2D (OffsetDelta2D { delta, chamfer }) children
    -> App "offset"
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
  Projection2D (RegularProjection2D { cut }) children
    -> App "projection"
         (concat
           [ optional (\c -> (Just "cut", LitBool c)) cut
           ]
         )
         (map toRawModel3D children)


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
           [ required (Just "points", LitVec $ map toRawVec3Double points)
           , optional (\f -> (Just "faces", LitVec $ map (LitVec . map LitInt) f)) faces
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
  Transform3D (Resize3D { newSize, auto }) children
    -> App "resize"
         (concat
           [ required (Just "newsize", toRawVec3Double newSize)
           , optional (\(a1, a2, a3) -> (Just "auto", LitVec [LitBool a1, LitBool a2, LitBool a3])) auto
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
  Extrude3D (LinearExtrude { height, center, twist, scale, slices, segments, convexity }) children
    -> App "linear_extrude"
         (concat
           [ required (Just "height", LitDouble height)
           , optional (\c -> (Just "center", LitBool c)) center
           , optional (\t -> (Just "twist", LitDouble t)) twist
           , optional (\s -> (Just "scale", LitDouble s)) scale
           , optional (\s -> (Just "slices", LitInt s)) slices
           , optional (\s -> (Just "segments", LitInt s)) segments
           , optional (\c -> (Just "convexity", LitInt c)) convexity
           ]
         )
         (map toRawModel2D children)
  Extrude3D (RotateExtrude { angle, start, convexity, _facets }) children
    -> App "rotate_extrude"
         (concat
           [ required (Just "angle", LitDouble angle)
           , required (Just "start", LitDouble start)
           , optional (\c -> (Just "convexity", LitInt c)) convexity
           , optionals toRawFacets _facets
           ]
         )
         (map toRawModel2D children)

toRawFacets :: Facets -> [ (Maybe String, Raw.Lit) ]
toRawFacets Facets { fa, fs, fn } =
    (maybe [] (\a -> [(Just "$fa", LitDouble a)]) fa) ++
    (maybe [] (\s -> [(Just "$fs", LitDouble s)]) fs) ++
    (maybe [] (\n -> [(Just "$fn", LitInt n)   ]) fn)

toRawVec3Double :: V3 Double -> Raw.Lit
toRawVec3Double (x, y, z) = LitVec [LitDouble x, LitDouble y, LitDouble z]

toRawVec2Double :: V2 Double -> Raw.Lit
toRawVec2Double (x, y) = LitVec [LitDouble x, LitDouble y]

-------------------------------------------------------------------------------
--- Render
-------------------------------------------------------------------------------

render3D :: Model3D -> String
render3D = Raw.render . toRawModel3D

render2D :: Model2D -> String
render2D = Raw.render . toRawModel2D

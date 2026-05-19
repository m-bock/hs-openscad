{- FOURMOLU_DISABLE -}

module OpenSCAD.Model
  ( Model3D(..)
  , Model2D(..)
  , Primitive2D(..)
  , Transform2D(..)
  , Projection2D(..)
  , Extrude3D(..)
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
  = Primitive2D  (Maybe Comment) Primitive2D
  | Transform2D  (Maybe Comment) Transform2D  [Model2D]
  | Projection2D (Maybe Comment) Projection2D [Model3D]

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
  = Primitive3D (Maybe Comment) Primitive3D
  | Transform3D (Maybe Comment) Transform3D [Model3D]
  | Extrude3D   (Maybe Comment) Extrude3D   [Model2D]

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
  Primitive2D comment (Circle2D { d, _facets })
    -> App comment "circle"
         (concat
           [ required (Just "d", LitDouble d)
           , optionals toRawFacets _facets
           ]
         )
         []
  Primitive2D comment (Square2D { size, center })
    -> App comment "square"
         (concat
           [ required (Just "size", toRawVec2Double size)
           , optional (\c -> (Just "center", LitBool c)) center
           ]
         )
         []
  Primitive2D comment (Polygon2D { points, paths, convexity })
    -> App comment "polygon"
         (concat
           [ required (Just "points", LitVec $ map toRawVec2Double points)
           , optional (\p -> (Just "paths", LitVec $ map (LitVec . map LitInt) p)) paths
           , optional (\c -> (Just "convexity", LitInt c)) convexity
           ]
         )
         []
  Transform2D comment (Scale2D { v2 }) children
    -> App comment "scale"
         (concat
           [ required (Just "v", toRawVec2Double v2)
           ]
         )
         (map toRawModel2D children)
  Transform2D comment (Resize2D { newSize, auto }) children
    -> App comment "resize"
         (concat
           [ required (Just "newsize", toRawVec2Double newSize)
           , optional (\(a1, a2) -> (Just "auto", LitVec [LitBool a1, LitBool a2])) auto
           ]
         )
         (map toRawModel2D children)
  Transform2D comment (RotateEuler2D { v2 }) children
    -> App comment "rotate"
         (concat
           [ required (Just "v", toRawVec2Double v2)
           ]
         )
         (map toRawModel2D children)
  Transform2D comment (RotateAxis2D { a, mv2 }) children
    -> App comment "rotate"
         (concat
           [ required (Just "a", LitDouble a)
           , optional (\v -> (Just "v", toRawVec2Double v)) mv2
           ]
         )
         (map toRawModel2D children)
  Transform2D comment (Translate2D { v3 }) children
    -> App comment "translate"
         (concat
           [ required (Just "v", toRawVec3Double v3)
           ]
         )
         (map toRawModel2D children)
  Transform2D comment (Mirror2D { v2 }) children
    -> App comment "mirror"
         (concat
           [ required (Just "v", toRawVec2Double v2)
           ]
         )
         (map toRawModel2D children)
  Transform2D comment (Color2D { c, alpha }) children
    -> App comment "color"
         (concat
           [ required (Just "c", toRawVec3Double c)
           , optional (\a -> (Just "alpha", LitDouble a)) alpha
           ]
         )
         (map toRawModel2D children)
  Transform2D comment (OffsetRadial2D { r }) children
    -> App comment "offset"
         (concat
           [ required (Just "r", LitDouble r)
           ]
         )
         (map toRawModel2D children)
  Transform2D comment (OffsetDelta2D { delta, chamfer }) children
    -> App comment "offset"
         (concat
           [ required (Just "delta", LitDouble delta)
           , optional (\c -> (Just "chamfer", LitBool c)) chamfer
           ]
         )
         (map toRawModel2D children)
  Transform2D comment (Fill2D) children
    -> App comment "fill"
         []
         (map toRawModel2D children)
  Transform2D comment (Minkowski2D) children
    -> App comment "minkowski"
         []
         (map toRawModel2D children)
  Transform2D comment (Hull2D) children
    -> App comment "hull"
         []
         (map toRawModel2D children)
  Transform2D comment (Union2D) children
    -> App comment "union"
         []
         (map toRawModel2D children)
  Transform2D comment (Intersection2D) children
    -> App comment "intersection"
         []
         (map toRawModel2D children)
  Transform2D comment (Difference2D) children
    -> App comment "difference"
         []
         (map toRawModel2D children)
  Projection2D comment (RegularProjection2D { cut }) children
    -> App comment "projection"
         (concat
           [ optional (\c -> (Just "cut", LitBool c)) cut
           ]
         )
         (map toRawModel3D children)


toRawModel3D :: Model3D -> Raw.Ast
toRawModel3D = \case
  Primitive3D comment (Cube3D { size })
    -> App comment "cube"
         (concat
           [ required (Just "size", toRawVec3Double size)
           ]
         )
         []
  Primitive3D comment (Cylinder3D { h, d1, d2, _facets })
    -> App comment "cylinder"
         (concat
           [ required (Just "h", LitDouble h)
           , required (Just "d1", LitDouble d1)
           , required (Just "d2", LitDouble d2)
           , optionals toRawFacets _facets
           ]
         )
         []
  Primitive3D comment (Sphere3D { d, _facets })
    -> App comment "sphere"
         (concat
           [ required (Just "d", LitDouble d)
           , optionals toRawFacets _facets
           ]
         )
         []
  Primitive3D comment (Polyhedron3D { points, faces, convexity })
    -> App comment "polyhedron"
         (concat
           [ required (Just "points", LitVec $ map toRawVec3Double points)
           , optional (\f -> (Just "faces", LitVec $ map (LitVec . map LitInt) f)) faces
           , optional (\c -> (Just "convexity", LitInt c)) convexity
           ]
         )
         []
  Transform3D comment (Scale3D { v }) children
    -> App comment "scale"
         (concat
           [ required (Just "v", toRawVec3Double v)
           ]
         )
         (map toRawModel3D children)
  Transform3D comment (Resize3D { newSize, auto }) children
    -> App comment "resize"
         (concat
           [ required (Just "newsize", toRawVec3Double newSize)
           , optional (\(a1, a2, a3) -> (Just "auto", LitVec [LitBool a1, LitBool a2, LitBool a3])) auto
           ]
         )
         (map toRawModel3D children)
  Transform3D comment (RotateEuler3D { v }) children
    -> App comment "rotate"
         (concat
           [ required (Just "v", toRawVec3Double v)
           ]
         )
         (map toRawModel3D children)
  Transform3D comment (RotateAxis3D { a, v }) children
    -> App comment "rotate"
         (concat
           [ required (Just "a", LitDouble a)
           , required (Just "v", toRawVec3Double v)
           ]
         )
         (map toRawModel3D children)
  Transform3D comment (Translate3D { v }) children
    -> App comment "translate"
         (concat
           [ required (Just "v", toRawVec3Double v)
           ]
         )
         (map toRawModel3D children)
  Transform3D comment (Mirror3D { v }) children
    -> App comment "mirror"
         (concat
           [ required (Just "v", toRawVec3Double v)
           ]
         )
         (map toRawModel3D children)
  Transform3D comment (Color3D { c, alpha }) children
    -> App comment "color"
         (concat
           [ optional (\co -> (Just "c", toRawVec3Double co)) c
           , optional (\a -> (Just "alpha", LitDouble a)) alpha
           ]
         )
         (map toRawModel3D children)
  Transform3D comment (Union3D) children
    -> App comment "union"
         []
         (map toRawModel3D children)
  Transform3D comment (Intersection3D) children
    -> App comment "intersection"
         []
         (map toRawModel3D children)
  Transform3D comment (Difference3D) children
    -> App comment "difference"
         []
         (map toRawModel3D children)
  Transform3D comment (Minkowski3D) children
    -> App comment "minkowski"
         []
         (map toRawModel3D children)
  Transform3D comment (Hull3D) children
    -> App comment "hull"
         []
         (map toRawModel3D children)
  Extrude3D comment (LinearExtrude { height, center, twist, scale, slices, segments, convexity }) children
    -> App comment "linear_extrude"
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
  Extrude3D comment (RotateExtrude { angle, start, convexity, _facets }) children
    -> App comment "rotate_extrude"
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

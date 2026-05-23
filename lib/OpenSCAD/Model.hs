{- FOURMOLU_DISABLE -}

module OpenSCAD.Model
  ( Model3D(..)
  , Model2D(..)
  , Primitive2D(..), Transform2D(..), Projection2D(..)
  , Primitive3D(..), Transform3D(..), Extrude3D(..)
  , Facets(..)
  , Font(..)
  , Direction(..), HorizontalAlignment(..), VerticalAlignment(..)
  , Modifier(..)
  , V3
  , V2
  , RGB
  , render3D
  , render2D
  ) where

-------------------------------------------------------------------------------
-- / Imports
-------------------------------------------------------------------------------

import OpenSCAD.Raw (Ast(..), Lit(..))
import qualified OpenSCAD.Raw as Raw
import Data.Monoid (First(..))
import Data.List (intercalate)

-------------------------------------------------------------------------------
-- / Types
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

data Direction = LeftToRight | RightToLeft | TopToBottom | BottomToTop
  deriving Eq

data HorizontalAlignment
  = HALeft
  | HACenter
  | HARight
  deriving Eq

data VerticalAlignment
  = VATop
  | VACenter
  | VABaseline
  | VABottom
  deriving Eq

data Modifier
  = ModDisable
  | ModShowOnly
  | ModHighlight
  | ModTransparent

-------------------------------------------------------------------------------
-- / Types / 2D
-------------------------------------------------------------------------------

data Model2D
  = Primitive2D  Primitive2D
  | Transform2D  Transform2D  [Model2D]
  | Projection2D Projection2D [Model3D]
  | Comment2D    Comment      Model2D
  | Modifier2D   Modifier     Model2D

data Projection2D = RegularProjection2D { cut :: Maybe Bool }

data Primitive2D
  = Circle2D
      { circleDiameter :: Double
      , circleFacets   :: Maybe Facets
      }
  | Square2D
      { squareSize   :: V2 Double
      , squareCenter :: Maybe Bool
      }
  | Polygon2D
      { polygonPoints    :: [V2 Double]
      , polygonPaths     :: Maybe [[Int]]
      , polygonConvexity :: Maybe Int
      }
  | Text2D
      { textText      :: String
      , textSize      :: Maybe Double
      , textFont      :: Maybe Font
      , textDirection :: Maybe Direction
      , textLanguage  :: Maybe String
      , textScript    :: Maybe String
      , textHAlign    :: Maybe HorizontalAlignment
      , textVAlign    :: Maybe VerticalAlignment
      , textSpacing   :: Maybe Double
      , textEm        :: Maybe Double
      , textFacets    :: Maybe Facets
      }

data Font = Font {
  fontFamily :: String,
  fontOptions :: [(String, String)]
}

data Transform2D
  = Scale2D
      { scaleVector :: V2 Double
      }
  | Resize2D
      { resizeNewSize :: V2 Double
      , resizeAuto :: Maybe (V2 Bool)
      }
  | RotateEuler2D
      { rotateEulerVector :: V2 Double
      }
  | RotateAxis2D
      { rotateAxisAngle :: Double
      , rotateAxisVector :: Maybe (V2 Double)
      }
  | Translate2D
      { translateVector :: V3 Double -- sic! 2d shapes can be translated in 3d space
      }
  | Mirror2D
      { mirrorVector :: V2 Double
      }
  | Color2D
      { colorColor :: RGB
      , colorAlpha :: Maybe Double
      }
  | OffsetRadial2D
      { offsetRadialRadius :: Double
      , offsetRadialFacets :: Maybe Facets
      }
  | OffsetDelta2D
      { offsetDeltaDelta :: Double
      , offsetDeltaChamfer :: Maybe Bool
      }
  | Fill2D
  | Minkowski2D
  | Hull2D
  | Union2D
  | Intersection2D
  | Difference2D

-------------------------------------------------------------------------------
-- / Types / 3D
-------------------------------------------------------------------------------

data Model3D
  = Primitive3D Primitive3D
  | Transform3D Transform3D [Model3D]
  | Extrude3D   Extrude3D   [Model2D]
  | Comment3D   Comment      Model3D
  | Modifier3D   Modifier     Model3D

data Extrude3D
  = LinearExtrude
      { linearHeight    :: Double
      , linearCenter    :: Maybe Bool
      , linearTwist     :: Maybe Double
      , linearScale     :: Maybe Double
      , linearSlices    :: Maybe Int
      , linearConvexity :: Maybe Int
      , linearFacets    :: Maybe Facets
      }
  | RotateExtrude
      { rotateAngle     :: Double
      , rotateStart     :: Double
      , rotateConvexity :: Maybe Int
      , rotateFacets    :: Maybe Facets
      }

data Primitive3D
  = Cube3D
      { cubeSize   :: V3 Double
      , cubeCenter :: Maybe Bool
      }
  | Cylinder3D
      { cylinderHeight    :: Double
      , cylinderDiameter1 :: Double
      , cylinderDiameter2 :: Double
      , cylinderCenter    :: Maybe Bool
      , cylinderFacets    :: Maybe Facets
      }
  | Sphere3D
      { sphereDiameter :: Double
      , sphereFacets   :: Maybe Facets
      }
  | Polyhedron3D
      { polyhedronPoints    :: [V3 Double]
      , polyhedronFaces     :: Maybe [[Int]]
      , polyhedronConvexity :: Maybe Int
      }

data Transform3D
  = Scale3D
      { scaleVector :: V3 Double
      }
  | Resize3D
      { resizeNewSize :: V3 Double
      , resizeAuto    :: Maybe (V3 Bool)
      }
  | RotateEuler3D
      { rotateEulerVector :: V3 Double
      }
  | RotateAxis3D
      { rotateAxisAngle  :: Double
      , rotateAxisVector :: V3 Double
      }
  | Translate3D
      { translateVector :: V3 Double }
  | Mirror3D
      { mirrorVector :: V3 Double }
  | Color3D
      { colorColor :: RGB
      , colorAlpha :: Maybe Double
      }
  | Union3D  
  | Intersection3D 
  | Difference3D
  | Minkowski3D
  | Hull3D
  
-------------------------------------------------------------------------------
-- / ToRaw / 2D
-------------------------------------------------------------------------------

toRawModel2D :: Model2D -> Raw.Ast
toRawModel2D = \case
  -- ** Primitive
  Primitive2D (Circle2D { circleDiameter, circleFacets })
    -> App "circle"
         (concat
           [ required "d" $ LitDouble circleDiameter
           , optionals toRawFacets circleFacets
           ]
         )
         []
  Primitive2D (Square2D { squareSize, squareCenter })
    -> App "square"
         (concat
           [ required "size"   $ toRawVec2Double squareSize
           , optional "center" $ fmap LitBool squareCenter
           ]
         )
         []
  Primitive2D (Polygon2D { polygonPoints, polygonPaths, polygonConvexity })
    -> App "polygon"
         (concat
           [ required "points"    $ LitVec $ map toRawVec2Double polygonPoints
           , optional "paths"     $ fmap (LitVec . map (LitVec . map LitInt)) polygonPaths
           , optional "convexity" $ fmap LitInt polygonConvexity
           ]
         )
         []
  Primitive2D (Text2D { textText, textSize, textFont, textDirection, textLanguage, textScript, textHAlign, textVAlign, textSpacing, textEm, textFacets })
      -> App "text"
          (concat
            [ required "text"      $ LitString textText
            , optional "size"      $ fmap LitDouble textSize
            , optional "font"      $ fmap toRawFont textFont
            , optional "direction" $ fmap toRawDirection textDirection
            , optional "language"  $ fmap LitString textLanguage
            , optional "script"    $ fmap LitString textScript
            , optional "halign"    $ fmap toRawHorizontalAlignment textHAlign
            , optional "valign"    $ fmap toRawVerticalAlignment textVAlign
            , optional "spacing"   $ fmap LitDouble textSpacing
            , optional "em"        $ fmap LitDouble textEm
            , optionals toRawFacets textFacets
           ]
         )
         []

  -- ** Transform
  Transform2D (Scale2D { scaleVector }) children
    -> App "scale"
         (concat
           [ required "v" $ toRawVec2Double scaleVector
           ]
         )
         (map toRawModel2D children)
  Transform2D (Resize2D { resizeNewSize, resizeAuto }) children
    -> App "resize"
         (concat
           [ required "newsize" $ toRawVec2Double resizeNewSize
           , optional "auto"    $ fmap (\(a1, a2) -> LitVec [LitBool a1, LitBool a2]) resizeAuto
           ]
         )
         (map toRawModel2D children)
  Transform2D (RotateEuler2D { rotateEulerVector }) children
    -> App "rotate"
         (concat
           [ required "a" $ toRawVec2Double rotateEulerVector
           ]
         )
         (map toRawModel2D children)
  Transform2D (RotateAxis2D { rotateAxisAngle, rotateAxisVector }) children
    -> App "rotate"
         (concat
           [ required "a" $ LitDouble rotateAxisAngle
           , optional "v" $ fmap toRawVec2Double rotateAxisVector
           ]
         )
         (map toRawModel2D children)
  Transform2D (Translate2D { translateVector }) children
    -> App "translate"
         (concat
           [ required "v" $ toRawVec3Double translateVector
           ]
         )
         (map toRawModel2D children)
  Transform2D (Mirror2D { mirrorVector }) children
    -> App "mirror"
         (concat
           [ required "v" $ toRawVec2Double mirrorVector
           ]
         )
         (map toRawModel2D children)
  Transform2D (Color2D { colorColor, colorAlpha }) children
    -> App "color"
         (concat
           [ required "c"     $ toRawVec3Double colorColor
           , optional "alpha" $ fmap LitDouble colorAlpha
           ]
         )
         (map toRawModel2D children)
  Transform2D (OffsetRadial2D { offsetRadialRadius, offsetRadialFacets }) children
    -> App "offset"
         (concat
           [ required "r" $ LitDouble offsetRadialRadius
           , optionals toRawFacets offsetRadialFacets
           ]
         )
         (map toRawModel2D children)
  Transform2D (OffsetDelta2D { offsetDeltaDelta, offsetDeltaChamfer }) children
    -> App "offset"
         (concat
           [ required "delta"   $ LitDouble offsetDeltaDelta
           , optional "chamfer" $ fmap LitBool offsetDeltaChamfer
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

  -- ** Projection
  Projection2D (RegularProjection2D { cut }) children
    -> App "projection"
         (concat
           [ optional "cut" $ fmap LitBool cut
           ]
         )
         (map toRawModel3D children)
 
  -- ** Comment
  Comment2D comment ast -> Comment comment (toRawModel2D ast)

  -- ** Modifier
  Modifier2D modifier ast -> Modifier (modifierToChar modifier) (toRawModel2D ast)

toRawDirection :: Direction -> Raw.Lit
toRawDirection = \case
  LeftToRight -> LitString "ltr"
  RightToLeft -> LitString "rtl"
  TopToBottom -> LitString "ttb"
  BottomToTop -> LitString "btt"

toRawHorizontalAlignment :: HorizontalAlignment -> Raw.Lit
toRawHorizontalAlignment = \case
  HALeft   -> LitString "left"
  HACenter -> LitString "center"
  HARight  -> LitString "right"

toRawVerticalAlignment :: VerticalAlignment -> Raw.Lit
toRawVerticalAlignment = \case
  VATop      -> LitString "top"
  VACenter   -> LitString "center"
  VABaseline -> LitString "baseline"
  VABottom   -> LitString "bottom"

toRawFont :: Font -> Raw.Lit
toRawFont Font { fontFamily, fontOptions } =
  LitString $ fontFamily <> " " <> intercalate "" (map (\(k, v) -> ":" ++ k ++ "=" ++ v) fontOptions)

-------------------------------------------------------------------------------
-- / ToRaw / 3D
-------------------------------------------------------------------------------

toRawModel3D :: Model3D -> Raw.Ast
toRawModel3D = \case
  -- ** Primitive
  Primitive3D (Cube3D { cubeSize, cubeCenter })
    -> App "cube"
         (concat
           [ required "size"   $ toRawVec3Double cubeSize
           , optional "center" $ fmap LitBool cubeCenter
           ]
         )
         []
  Primitive3D (Cylinder3D { cylinderHeight, cylinderDiameter1, cylinderDiameter2, cylinderCenter, cylinderFacets })
    -> App "cylinder"
         (concat
           [ required "h"      $ LitDouble cylinderHeight
           , required "d1"     $ LitDouble cylinderDiameter1
           , required "d2"     $ LitDouble cylinderDiameter2
           , optional "center" $ fmap LitBool cylinderCenter
           , optionals toRawFacets cylinderFacets
           ]
         )
         []
  Primitive3D (Sphere3D { sphereDiameter, sphereFacets })
    -> App "sphere"
         (concat
           [ required "d" $ LitDouble sphereDiameter
           , optionals toRawFacets sphereFacets
           ]
         )
         []
  Primitive3D (Polyhedron3D { polyhedronPoints, polyhedronFaces, polyhedronConvexity })
    -> App "polyhedron"
         (concat
           [ required "points" $ LitVec $ map toRawVec3Double polyhedronPoints
           , optional "faces" $ fmap (LitVec . map (LitVec . map LitInt)) polyhedronFaces
           , optional "convexity" $ fmap LitInt polyhedronConvexity
           ]
         )
         []

  -- ** Transform
  Transform3D (Scale3D { scaleVector }) children
    -> App "scale"
         (concat
           [ required "v" $ toRawVec3Double scaleVector
           ]
         )
         (map toRawModel3D children)
  Transform3D (Resize3D { resizeNewSize, resizeAuto }) children
    -> App "resize"
         (concat
           [ required "newsize" $ toRawVec3Double resizeNewSize
           , optional "auto"    $ fmap (\(a1, a2, a3) -> LitVec [LitBool a1, LitBool a2, LitBool a3]) resizeAuto
           ]
         )
         (map toRawModel3D children)
  Transform3D (RotateEuler3D { rotateEulerVector }) children
    -> App "rotate"
         (concat
           [ required "a" $ toRawVec3Double rotateEulerVector
           ]
         )
         (map toRawModel3D children)
  Transform3D (RotateAxis3D { rotateAxisAngle, rotateAxisVector }) children
    -> App "rotate"
         (concat
           [ required "a" $ LitDouble rotateAxisAngle
           , required "v" $ toRawVec3Double rotateAxisVector
           ]
         )
         (map toRawModel3D children)
  Transform3D (Translate3D { translateVector }) children
    -> App "translate"
         (concat
           [ required "v" $ toRawVec3Double translateVector
           ]
         )
         (map toRawModel3D children)
  Transform3D (Mirror3D { mirrorVector }) children
    -> App "mirror"
         (concat
           [ required "v" $ toRawVec3Double mirrorVector
           ]
         )
         (map toRawModel3D children)
  Transform3D (Color3D { colorColor, colorAlpha }) children
    -> App "color"
         (concat
           [ required "c" $ toRawVec3Double colorColor
           , optional "alpha" $ fmap LitDouble colorAlpha
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

  -- ** Extrude
  Extrude3D (LinearExtrude { linearHeight, linearCenter, linearTwist, linearScale, linearSlices, linearConvexity }) children
    -> App "linear_extrude"
         (concat
           [ required "height"    $ LitDouble linearHeight
           , optional "center"    $ fmap LitBool linearCenter
           , optional "twist"     $ fmap LitDouble linearTwist
           , optional "scale"     $ fmap LitDouble linearScale
           , optional "slices"    $ fmap LitInt linearSlices
           , optional "convexity" $ fmap LitInt linearConvexity
           ]
         )
         (map toRawModel2D children)
  Extrude3D (RotateExtrude { rotateAngle, rotateStart, rotateConvexity, rotateFacets }) children
    -> App "rotate_extrude"
         (concat
           [ required "angle"     $ LitDouble rotateAngle
           , required "start"     $ LitDouble rotateStart
           , optional "convexity" $ fmap LitInt rotateConvexity
           , optionals toRawFacets rotateFacets
           ]
         )
         (map toRawModel2D children)

  -- ** Comment
  Comment3D comment ast
    -> Comment comment (toRawModel3D ast)

  -- ** Modifier
  Modifier3D modifier ast
    -> Modifier (modifierToChar modifier) (toRawModel3D ast)

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
-- / Helpers
-------------------------------------------------------------------------------

optionals :: (a -> [b]) -> Maybe a -> [b]
optionals f = maybe [] f

required :: String -> a -> [(Maybe String, a)]
required name x = [(Just name, x)]

optional :: String -> Maybe a -> [(Maybe String, a)]
optional name x = case x of
  Just x -> [(Just name, x)]
  Nothing -> []

modifierToChar :: Modifier -> Char
modifierToChar = \case
  ModDisable -> '*'
  ModShowOnly -> '!'
  ModHighlight -> '#'
  ModTransparent -> '%'

-------------------------------------------------------------------------------
-- / Render
-------------------------------------------------------------------------------

render3D :: Model3D -> String
render3D = Raw.render . toRawModel3D

render2D :: Model2D -> String
render2D = Raw.render . toRawModel2D

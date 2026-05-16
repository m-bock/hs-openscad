module OpenSCAD.Model3D (Model3D(..), Facet(..), V3, render) where

import OpenSCAD.Raw (Ast(..))
import qualified OpenSCAD.Raw as Raw

data Facet = MinAngle Double | MinSize Double | NumFacets Int | Def
  deriving Show

type V3 = (Double, Double, Double)

data Model3D
  = Cube V3
  | Cylinder {h :: Double, d1 :: Double, d2 :: Double, facet :: Facet}
  | Sphere { diameter :: Double }
  | Scale V3 Model3D
  | Resize V3 Model3D
  | RotateEuler V3 Model3D
  | RotateAxis Double V3 Model3D
  | Translate V3 Model3D
  | Mirror V3 Model3D
  | Union [Model3D]
  | Intersection [Model3D]
  | Minkowski [Model3D]
  | Hull [Model3D]
  | Difference Model3D [Model3D]

toRaw :: Model3D -> Raw.Ast
toRaw = \case
  Cube (x, y, z)
    -> App "cube"
         [(Just "size", Vec [LitDouble x, LitDouble y, LitDouble z])]
         []
  Cylinder h d1 d2 facet
    -> App "cylinder"
         ([(Just "h", LitDouble h), (Just "d1", LitDouble d1), (Just "d2", LitDouble d2)] ++ toRawFacet facet)
         []
  Sphere d
    -> App "sphere"
         [(Just "d", LitDouble d)]
         []
  Scale v model
    -> App "scale"
         [(Just "v", toRawVec v)]
         [toRaw model]
  Resize v model
    -> App "resize"
         [(Just "v", toRawVec v)]
         [toRaw model]
  RotateEuler v model
    -> App "rotate_euler"
         [(Just "v", toRawVec v)]
         [toRaw model]
  RotateAxis angle v model
    -> App "rotate_axis"
         [(Just "angle", LitDouble angle), (Just "v", toRawVec v)]
         [toRaw model]
  Translate v model
    -> App "translate"
         [(Just "v", toRawVec v)]
         [toRaw model]
  Mirror v model
    -> App "mirror"
         [(Just "v", toRawVec v)]
         [toRaw model]
  Union models      
    -> App "union"
         []
         (fmap toRaw models)
  Intersection models
    -> App "intersection"
         []
         (fmap toRaw models)
  Minkowski models
    -> App "minkowski"
         []
         (fmap toRaw models)
  Hull models
    -> App "hull"
         []
         (fmap toRaw models)
  Difference model models
    -> App "difference"
         []
         (toRaw model : fmap toRaw models)
  
toRawFacet :: Facet -> [ (Maybe String, Raw.Ast) ]
toRawFacet = \case
  MinAngle angle -> [ (Just "$fa", LitDouble angle) ]
  MinSize size   -> [ (Just "$fs", LitDouble size) ]
  NumFacets num  -> [ (Just "$fn", LitInt num) ]
  Def            -> []

toRawVec :: V3 -> Raw.Ast
toRawVec (x, y, z) = Vec [LitDouble x, LitDouble y, LitDouble z]

render :: Model3D -> String
render = Raw.render . toRaw

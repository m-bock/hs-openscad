module OpenSCAD (
    module OpenSCAD.Model,
    render3D,
    render2D,
) where

import OpenSCAD.Model
import qualified OpenSCAD.Raw as Raw

-------------------------------------------------------------------------------
-- / Render
-------------------------------------------------------------------------------

render3D :: Model3D -> String
render3D = Raw.render . toRawModel3D

render2D :: Model2D -> String
render2D = Raw.render . toRawModel2D

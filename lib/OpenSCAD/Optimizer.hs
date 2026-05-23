{- FOURMOLU_DISABLE -}

module OpenSCAD.Optimizer where

import OpenSCAD.Model

-- optimize3D :: Model3D -> Model3D
-- optimize3D = \case
--     Primitive3D primitive          -> Primitive3D primitive
    
--     Transform3D t1 [Transform3D (Union3D) ch] | t1 == t2 -> Transform3D t1 []
    
--     Transform3D transform children -> Transform3D transform (map optimize3D children)
--     Extrude3D extrude children     -> Extrude3D extrude (map optimize2D children)
--     Comment3D comment ast          -> Comment3D comment (optimize3D ast)
--     Modifier3D modifier ast        -> Modifier3D modifier (optimize3D ast)

-- optimize2D :: Model2D -> Model2D
-- optimize2D = \case
--     Primitive2D primitive            -> Primitive2D primitive
--     Transform2D transform children   -> Transform2D transform (map optimize2D children)
--     Projection2D projection children -> Projection2D projection (map optimize3D children)
--     Comment2D comment ast            -> Comment2D comment (optimize2D ast)
--     Modifier2D modifier ast          -> Modifier2D modifier (optimize2D ast)

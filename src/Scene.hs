{-# LANGUAGE PatternSynonyms, ViewPatterns, ScopedTypeVariables #-}
module Scene where

import Vector
import Data.Array.Accelerate

type Material = (Vector3, Double, Vector3)

pattern Material col dif glo <- (unlift -> (col, dif, glo) :: (Exp Vector3, Exp Double, Exp Vector3)) where
    Material col dif glo = lift (col, dif, glo)

_color :: Exp Material -> Exp Vector3
_color (Material col _ _) = col

_diffuse :: Exp Material -> Exp Double
_diffuse (Material _ dif _) = dif

_glow :: Exp Material -> Exp Vector3
_glow (Material _ _ glo) = glo

type Sphere = (Vector3, Double, Material)

pattern Sphere pos rad mat <- (unlift -> (pos, rad, mat) :: (Exp Vector3, Exp Double, Exp Material)) where
    Sphere pos rad mat = lift (pos, rad, mat)

_spherePos :: Exp Sphere -> Exp Vector3
_spherePos (Sphere pos _ _) = pos

_radius :: Exp Sphere -> Exp Double
_radius (Sphere _ rad _) = rad

_sphereMat :: Exp Sphere -> Exp Material
_sphereMat (Sphere _ _ mat) = mat

type Scene = Vector Sphere

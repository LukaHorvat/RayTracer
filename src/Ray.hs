{-# LANGUAGE ViewPatterns, PatternSynonyms, ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances #-}
module Ray where

import Prelude hiding (fst, snd, map, zip, null, filter, (<*))
import Scene
import Vector
import Data.Maybe (mapMaybe)
import Data.List (minimumBy)
import Data.Ord (comparing)
import Data.Fixed (mod')
import Data.Array.Accelerate hiding (intersect)

type Ray = (Vector3, Vector3)

pattern Ray start dir <- (unlift -> (start, dir) :: (Exp Vector3, Exp Vector3)) where
    Ray start dir = lift (start, dir)

_start :: Exp Ray -> Exp Vector3
_start = fst

_direction :: Exp Ray -> Exp Vector3
_direction = snd
{-
    (x - xs)^2 + (y - ys)^2 + (z - zs)^2 = r^2
    x(t) = xr + t * xv
    y(t) = yr + t * yv
    z(t) = zr + t * zv

    (xr - xs + t * xv)^2 + (yr - ys + t * yv)^2 + (zr - zs + t * zv)^2 = r^2

    t^2(xv^2 + yv^2 + zv^2) + 2t(xv(xr - xs) + yv(yr - ys) + zv(zr - zs)) +
    (xr - xs)^2 + (yr - ys)^2 + (zr - zs)^2 - r^2 = 0

    a = xv^2 + yv^2 + zv^2
    b = 2(xv(xr - xs) + yv(yr - ys) + zv(zr - zs))
    c = (xr - xs)^2 + (yr - ys)^2 + (zr - zs)^2 - r^2

    b^2 - 4ac > 0

    t1,2 = (-b +- sqrt(b^2 - 4ac)) / 2a
-}

type Optional a = (Bool, a)

some :: Elt a => Exp a -> Exp (Optional a)
some val = lift (lift True :: Exp Bool, val)

class Elt a => None a where
    zeroExp :: Exp a

none :: None a => Exp (Optional a)
none = lift (lift False :: Exp Bool, zeroExp)

instance None Vector3 where
    zeroExp = V3 0 0 0

instance None Ray where
    zeroExp = lift (V3 0 0 0, V3 0 0 0)

intersect :: Exp Ray -> Exp Sphere -> Exp (Optional Vector3)
intersect (Ray (V3 xr yr zr) (V3 xv yv zv)) (Sphere (V3 xs ys zs) r _)
    | cond && t > 0.1 = some $ V3 (xr + t * xv) (yr + t * yv) (zr + t * zv)
    | otherwise       = none
    where (xd, yd, zd) = (xr - xs, yr - ys, zr - zs)
          a = xv ** 2 + yv ** 2 + zv ** 2
          b = 2 * (xv * xd + yv * yd + zv * zd)
          c = xd ** 2 + yd ** 2 + zd ** 2 - r ** 2
          d = b ** 2 - 4 * a * c
          cond = d > 0
          t = min ((-b + sqrt d) / (a * 2)) ((-b - sqrt d) / (a * 2))

{-
(a' - a) = kn
a' = kn + a

(x', y', z') = (k * nx, k * ny, k * nz) + (x, y, z)
(x', y', z') = (k * nx + x, k * ny + y, k * nz + z)
x'^2 + y'^2 + z'^2 = x^2 + y^2 + z^2

(k * nx + x)^2 + (k * ny + y)^2 + (k * nz + z)^2 = x^2 + y^2 + z^2
k^2(nx^2 + ny^2 + nz^2) + 2k(x * nx + y * ny + z * nz) = 0
k(nx^2 + ny^2 + nz^2) + 2(x * nx + y * ny + z * nz) = 0
k = -2(x * nx + y * ny + z * nz)/(nx^2 + ny^2 + nz^2)
-}
bounce :: Exp Vector3 -> Exp Vector3 -> Exp Vector3 -> Exp Ray
bounce (V3 x y z) sphereC inter = Ray inter res
    where V3 nx ny nz = inter .-. sphereC
          k = (-2) * (x * nx + y * ny + z * nz) / (nx ** 2 + ny ** 2 + nz ** 2)
          res = V3 (k * nx + x) (k * ny + y) (k * nz + z)

type Color = Vector3

minIntersect :: Exp ((Vector3, Sphere), Double) -> Exp ((Vector3, Sphere), Double) -> Exp ((Vector3, Sphere), Double)
minIntersect a@(unlift -> (_ :: Exp (Vector3, Sphere), ad :: Exp Double))
             b@(unlift -> (_ :: Exp (Vector3, Sphere), bd :: Exp Double)) =
                 cond (ad <* bd) a b

cast :: Exp Ray -> Acc Scene -> Exp (Color, Optional Ray)
cast ray' spheres = cast' ray'
    where cast' :: Exp Ray -> Exp (Color, Optional Ray)
          cast' ray = cond (null intersections)
              (lift (V3 0 0 0, none))
              (lift (V3 red green blue, some reflect))
              where pairs = map (\s -> lift (ray `intersect` s, s)) spheres
                    intersections = map (\p -> lift (snd (fst p), snd p)) $ filter (fst . fst) pairs
                    start = _start ray
                    dists = map (sqMagnitude . (.-.) start . fst) intersections
                    expVecSphere = fst $ the $ fold1All minIntersect $ zip intersections dists
                    (V3 xi yi zi, Sphere c _ m) = unlift expVecSphere :: (Exp Vector3, Exp Sphere)
                    V3 red green blue = _glow m
                    d = _diffuse m
                    i' = V3 (xi + (mod' (xi * 123456789) 2 - 1) * d)
                            (yi + (mod' (yi * 123456789) 2 - 1) * d)
                            (zi + (mod' (zi * 123456789) 2 - 1) * d)
                    reflect = bounce (_direction ray) c i'

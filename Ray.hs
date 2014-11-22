module Ray where

import Scene
import Vector
import Data.Maybe (mapMaybe)
import Data.List (minimumBy)
import Data.Ord (comparing)
import Graphics.UI.GLUT (Color3(..), GLfloat)

data Ray = Ray
         { _start :: Vector3
         , _direction :: Vector3 }

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
intersect :: Ray -> Sphere -> Maybe Vector3
intersect ray sphere
    | cond && t > 0 = Just $ Vector3 (xr + t * xv, yr + t * yv, zr + t * zv)
    | otherwise     = Nothing
    where (Ray (Vector3 (xr, yr, zr)) (Vector3 (xv, yv, zv))) = ray
          (Sphere (Vector3 (xs, ys, zs)) r _) = sphere
          (xd, yd, zd) = (xr - xs, yr - ys, zr - zs)
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
bounce :: Vector3 -> Vector3 -> Vector3 -> Ray
bounce (Vector3 (x, y, z)) sphereC inter = Ray inter res
    where (Vector3 (nx, ny, nz)) = inter .-. sphereC
          k = (-2) * (x * nx + y * ny + z * nz) / (nx ** 2 + ny ** 2 + nz ** 2)
          res = Vector3 (k * nx + x, k * ny + y, k * nz + z)

cast :: Ray -> Scene -> Color3 GLfloat
cast ray' (Scene spheres) = cast' 0 ray'
    where cast' :: Int -> Ray -> Color3 GLfloat
          cast' iter ray
              | null intersections || iter > 2 = Color3 0 0 0
              | otherwise                      = Color3 (red + recR) (green + recG) (blue + recB)
              where pairs = map (\s -> (ray `intersect` s, s)) spheres
                    intersections = mapMaybe (\(mi, s) -> fmap (\i -> (i, s)) mi) pairs
                    start = _start ray
                    dists = map (sqMagnitude . (.-.) start . fst) intersections
                    (i', Sphere c _ m) = fst $ minimumBy (comparing snd) $ zip intersections dists 
                    (Color3 red green blue) = _glow m
                    reflect = bounce (_direction ray) c i'
                    (Color3 recR recG recB) = cast' (iter + 1) reflect


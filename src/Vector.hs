{-# LANGUAGE PatternSynonyms, TypeSynonymInstances, FlexibleInstances, ViewPatterns
           , ScopedTypeVariables #-}
module Vector where

import Data.Array.Accelerate

type Vector3 = (Double, Double, Double)

packVector3 :: (Exp Double, Exp Double, Exp Double) -> Exp Vector3
packVector3 = lift

unpackVector3 :: Exp Vector3 -> (Exp Double, Exp Double, Exp Double)
unpackVector3 = unlift

pattern V3 x y z <- (unpackVector3 -> (x, y, z)) where
    V3 x y z = packVector3 (x, y, z)

(.+.) :: Exp Vector3 -> Exp Vector3 -> Exp Vector3
V3  x1 y1 z1 .+. V3  x2 y2 z2 = V3 (x1 + x2) (y1 + y2) (z1 + z2)

opposite :: Exp Vector3 -> Exp Vector3
opposite (V3 x y z) = V3 (-x) (-y) (-z)

dot :: Exp Vector3 -> Exp Vector3 -> Exp Double
V3 x1 y1 z1 `dot` V3 x2 y2 z2 = x1 * x2 + y1 * y2 + z1 * z2

scale :: Exp Vector3 -> Exp Double -> Exp Vector3
V3 x y z `scale` a = V3 (x * a) (y * a) (z * a)

(.-.) :: Exp Vector3 -> Exp Vector3 -> Exp Vector3
v1 .-. v2 = v1 .+. opposite v2

sqMagnitude :: Exp Vector3 -> Exp Double
sqMagnitude v = v `dot` v

magnitude :: Exp Vector3 -> Exp Double
magnitude v1 = sqrt $ sqMagnitude v1

normalize :: Exp Vector3 -> Exp Vector3
normalize v = v `scale` (1 / magnitude v)

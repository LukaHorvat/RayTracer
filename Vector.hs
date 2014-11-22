{-# LANGUAGE PatternSynonyms #-}
module Vector where

import Graphics.UI.GLUT hiding (scale, Vector3)

class Vec a where
    (.+.) :: a -> a -> a
    opposite :: a -> a
    dot :: a -> a -> GLdouble
    scale :: a -> GLdouble -> a

newtype Vector = Vector (GLdouble, GLdouble)
    deriving Show

instance Vec Vector where
    (Vector (x1, y1)) .+. (Vector (x2, y2)) = Vector (x1 + x2, y1 + y2)
    opposite (Vector (x, y)) = Vector (-x, -y)
    (Vector (x1, y1)) `dot` (Vector (x2, y2)) = x1  * x2 + y1 * y2 
    (Vector (x, y)) `scale` a = Vector (x * a, y * a)

newtype Vector3 = Vector3 (GLdouble, GLdouble, GLdouble)
    deriving Show

instance Vec Vector3 where
    (Vector3 (x1, y1, z1)) .+. (Vector3 (x2, y2, z2)) = Vector3 (x1 + x2, y1 + y2, z1 + z2)
    opposite (Vector3 (x, y, z)) = Vector3 (-x, -y, -z)
    (Vector3 (x1, y1, z1)) `dot` (Vector3 (x2, y2, z2)) = x1  * x2 + y1 * y2 + z1 * z2
    (Vector3 (x, y, z)) `scale` a = Vector3 (x * a, y * a, z * a)

(.-.) :: Vec a => a -> a -> a
v1 .-. v2 = v1 .+. opposite v2

sqMagnitude :: Vec a => a -> GLdouble
sqMagnitude v = v `dot` v

magnitude :: Vec a => a -> GLdouble
magnitude v1 = sqrt $ sqMagnitude v1

normalize :: Vec a => a -> a
normalize v = v `scale` (1 / magnitude v)

pattern V x y = Vector (x, y)
pattern V3 x y z = Vector3 (x, y, z)
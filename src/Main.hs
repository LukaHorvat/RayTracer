{-# LANGUAGE PatternSynonyms #-}
module Main where

import Prelude

import qualified Data.Array.IArray as IArray
import Data.Array.Accelerate hiding (fromIntegral)
import qualified Data.Array.Accelerate.CUDA as CUDA
import Data.List (foldl1')
import Graphics.UI.GLUT hiding (initialize, Sphere, Vector3, index)
import Vector
import Game
import Control.Monad
import Ray
import Scene

red :: Material
red = Material (Color3 1 0 0) 0 (1, 0, 0)

none :: Material
none = Material (Color3 1 0 0) 0.0 (0.1, 0.1, 0.1)

scene :: Scene
scene = Scene [ Sphere (Vector3 (-4, 0, 5 )) 2 none
              , Sphere (Vector3 (4,  0, 5 )) 2 red
              , Sphere (Vector3 (0,  1, 7 )) 1 none
              , Sphere (Vector3 (0,  3, 10)) 2 none ]

res :: Int
res = 300

resE :: Exp Int
resE = 300

rpp :: Int
rpp = 2

rppE :: Exp Int
rppE = 2

type State = (GLdouble, GLdouble, GLdouble)

render :: (GLdouble, GLdouble, GLdouble) -> IO ()
{-# INLINE render #-}
render (cx, cy, cz) = do
    let d :: Int -> Double
        d n = fromIntegral n / fromIntegral (res * rpp) * 2.0 - 1.0
        generateCell :: DIM2 -> (Double, Double, Double)
        generateCell (Z :. i :. j) = cast ray scene
            where ray = Ray (Vector3 (cx, cy, cz)) (Vector3 (cx + x, cy + y, cz + 1.0))
                  (x, y) = (d j, d i)
        expanded = CUDA.run $ generate (index2 (resE * rppE) (resE * rppE)) (lift1 generateCell) :: Array DIM2 (Double, Double, Double)
        addC (r1, g1, b1) (r2, g2, b2) = (r1 + r2, g1 + g2, b1 + b2)
        divC (r, g, b) n = let x = fromIntegral n in (r / x, g / x, b / x)
        avg (Z :. i :. j) = (lift2 divC) (foldl1' (lift2 addC) block) (rpp * rpp)
            where range = [0..rpp - 1]
                  block = [expanded ! (Z :. (i * rpp + y) :. (j * rpp + x)) | x <- range, y <- range]
    let reduced :: IArray.Array (Int, Int) (Double, Double, Double)
        reduced = toIArray (generate (Z :. res :. res) avg)
    let points = [(i, j) | let s = [0..res - 1], i <- s, j <- s]
    renderPrimitive Points $ forM_ points $ \(i, j) -> do
        when (i `mod` 100 == 0 && j == 0) (print i)
        let (x, y) = (fromIntegral j / fromIntegral res * 2.0 - 1.0, fromIntegral i / fromIntegral res * 2.0 - 1.0)
        let (r, g, b) = reduced IArray.! (i, j)
        color $ Color3 (realToFrac r :: GLdouble) (realToFrac g) (realToFrac b)
        vertex $ Vertex3 (x :: GLdouble) (y :: GLdouble) 0

input :: Key -> KeyState -> Modifiers -> Position -> State -> State
input key _ _ _ (x, y, z)
    | key == Char 'a' = (x - 0.1, y, z)
    | key == Char 'd' = (x + 0.1, y, z)
    | key == Char 'w' = (x, y, z + 0.1)
    | key == Char 's' = (x, y, z - 0.1)
    | otherwise       = (x, y, z)

main :: IO ()
main = run (return ())
           (0, 0, 0)
           (const id)
           input
           render
           0

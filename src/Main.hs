{-# LANGUAGE PatternSynonyms, ViewPatterns, FlexibleContexts #-}
module Main where

import Prelude hiding (fst, fromIntegral, map)
import qualified Prelude as P

import qualified Data.Array.IArray as IArray
import Data.Array.Accelerate
import qualified Data.Array.Accelerate.CUDA as CUDA
import Data.List (foldl1')
import Graphics.UI.GLUT hiding (initialize, Sphere, Vector3, index, scale, Clamp, Color)
import Vector
import Game
import Control.Monad
import Ray
import Scene
import Control.Exception

red :: Material
red = ((1, 0, 0), 0, (1, 0, 0))

noColor :: Material
noColor = ((1, 0, 0), 0.0, (0.1, 0.1, 0.1))

scene :: Acc Scene
scene = use $ fromList (Z :. 4) [ ((-4, 0, 5 ), 2, noColor)
                                , (( 4, 0, 5 ), 2, red)
                                , (( 0, 3, 10), 2, noColor)
                                , (( 0, 1, 7 ), 1, red) ]

res :: Int
res = 300

resE :: Exp Int
resE = 300

rpp :: Int
rpp = 2

rppE :: Exp Int
rppE = 2

type State = (GLdouble, GLdouble, GLdouble)

d :: Exp Int -> Exp Double
d n = fromIntegral n / fromIntegral (resE * rppE) * 2.0 - 1.0

generateCell :: (Exp Double, Exp Double, Exp Double) -> Exp DIM2 -> Exp (Color, Optional Ray)
generateCell (cx, cy, cz) (unlift -> Z :. i :. j) = pack (zeroExp, some ray)
    where ray = Ray (V3 cx cy cz) (V3 (cx + x) (cy + y) (cz + 1.0))
          (x, y) = (d j, d i)

avg :: Stencil3x3 Vector3 -> Exp Vector3
avg ((x11, x12, x13), (x21, x22, x23), (x31, x32, x33)) =
    foldl1' (.+.) [x11, x12, x13, x21, x22, x23, x31, x32, x33] `scale` (1 / 9)

singleIteration :: Acc (Array DIM2 (Color, Optional Ray)) -> Acc (Array DIM2 (Color, Optional Ray))
singleIteration = map f
    where f opt@(unpack -> (c, unpack -> (b, ray))) =
              cond b (cast c ray scene) opt

applyN :: Int -> (a -> a) -> a -> a
applyN 0 _ x = x
applyN n f x = f (applyN (n - 1) f x)

initialRays :: Exp DIM2 -> Exp Vector3 -> Acc (Array DIM2 (Color, Optional Ray))
initialRays sh c = generate sh (generateCell (unpack c))

generateScreen :: Acc (Scalar Vector3) -> Acc (Array DIM2 Vector3)
generateScreen inp = stencil avg Clamp colors
    where screenSize = index2 (resE * rppE) (resE * rppE)
          expanded   = applyN 5 singleIteration $ initialRays screenSize (the inp)
          colors     = map fst expanded

generateScreenCompiled = CUDA.run1 generateScreen

single a = fromList Z [a]

render :: (GLdouble, GLdouble, GLdouble) -> IO ()
{-# INLINE render #-}
render c = do
    let reduced :: IArray.Array (Int, Int) (Double, Double, Double)
        reduced = toIArray $ generateScreenCompiled (single c)
        points = [(i, j) | let s = [0..res - 1], i <- s, j <- s]
    renderPrimitive Points $ forM_ points $ \(i, j) -> do
        when (i `mod` 100 == 0 && j == 0) (print i)
        let (x, y) = (P.fromIntegral j / P.fromIntegral res * 2.0 - 1.0, P.fromIntegral i / P.fromIntegral res * 2.0 - 1.0)
        let (r, g, b) = reduced IArray.! (i * 2, j * 2)
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

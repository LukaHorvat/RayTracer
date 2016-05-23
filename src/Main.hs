{-# LANGUAGE PatternSynonyms, ViewPatterns, FlexibleContexts #-}
module Main where

import Prelude hiding (fst, fromIntegral, map)
import qualified Prelude as P

import qualified Data.Array.IArray as IArray
import Data.Array.Accelerate
import qualified Data.Array.Accelerate.CUDA as CUDA
import Data.List (foldl1')
import Graphics.UI.GLUT hiding (initialize, Sphere, Vector3, index, scale, Clamp)
import Vector
import Game
import Control.Monad
import Ray
import Scene

red :: Material
red = ((1, 0, 0), 0, (1, 0, 0))

noColor :: Material
noColor = ((1, 0, 0), 0.0, (0.1, 0.1, 0.1))

scene :: Acc Scene
scene = use $ fromList (Z :. 4) [ ((-4, 0, 5 ), 2, noColor)
                                , (( 4, 0, 5 ), 2, red)
                                , (( 0, 1, 7 ), 1, noColor)
                                , (( 0, 3, 10), 2, noColor) ]

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

generateCell :: (Exp Double, Exp Double, Exp Double)
             -> Exp DIM2 -> Exp ((Double, Double, Double), Optional Ray)
generateCell (cx, cy, cz) (unlift -> Z :. i :. j) = cast ray scene
    where ray = Ray (V3 cx cy cz) (V3 (cx + x) (cy + y) (cz + 1.0))
          (x, y) = (d j, d i)

generate' :: Elt a => Exp DIM2 -> (Exp DIM2 -> Exp a) -> Acc (Array DIM2 a)
generate' sh f = map f $ generate sh id

-- avg :: Acc (Array DIM2 Vector3) -> Exp DIM2 -> Exp Vector3
-- avg arr (unlift -> Z :. i :. j) = foldl1' (.+.) block `scale` fromIntegral (rppE * rppE)
--     where range = [0..rpp - 1]
--           block = [arr ! index2 (i * rppE + lift y) (j * rppE + lift x) | x <- range, y <- range]

avg :: Stencil3x3 Vector3 -> Exp Vector3
avg ((x11, x12, x13), (x21, x22, x23), (x31, x32, x33)) =
    foldl1' (.+.) [x11, x12, x13, x21, x22, x23, x31, x32, x33] `scale` (1 / 9)

render :: (GLdouble, GLdouble, GLdouble) -> IO ()
{-# INLINE render #-}
render (cx', cy', cz') = do
    let c = (lift cx', lift cy', lift cz')
        expanded = generate' (index2 (resE * rppE) (resE * rppE)) (fst . generateCell c)
        reduced :: IArray.Array (Int, Int) (Double, Double, Double)
        reduced = toIArray $ CUDA.run (stencil avg Clamp expanded)
        points = [(i * 2, j * 2) | let s = [0..res - 1], i <- s, j <- s]
    renderPrimitive Points $ forM_ points $ \(i, j) -> do
        when (i `mod` 100 == 0 && j == 0) (print i)
        let (x, y) = (P.fromIntegral j / P.fromIntegral res * 2.0 - 1.0, P.fromIntegral i / P.fromIntegral res * 2.0 - 1.0)
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

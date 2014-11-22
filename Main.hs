{-# LANGUAGE PatternSynonyms #-}
module Main where

import Data.Vector ((!))
import Data.List (foldl1')
import qualified Data.Vector as DVector
import Graphics.UI.GLUT hiding (initialize, Sphere, Vector3)
import Vector
import Game
import Control.Monad
import Ray
import Scene

red :: Material
red = Material (Color3 1 0 0) 0 (Color3 1 0 0)

none :: Material
none = Material (Color3 1 0 0) 0 (Color3 0.1 0.1 0.1)

scene :: Scene
scene = Scene [ Sphere (Vector3 (-4, 0, 5)) 2 none
              , Sphere (Vector3 (4, 0, 5)) 2 red 
              , Sphere (Vector3 (0, 1, 7)) 1 none
              , Sphere (Vector3 (0, 3, 10)) 2 none ]

res :: Int
res = 300

rpp :: Int
rpp = 4

type State = (GLdouble, GLdouble, GLdouble)

render :: (GLdouble, GLdouble, GLdouble) -> IO ()
render (cx, cy, cz) = do
    let d n = fromIntegral n / fromIntegral (res * rpp) * 2.0 - 1.0 
    let generateCell i j = cast ray scene
            where ray = Ray (Vector3 (cx, cy, cz)) (Vector3 (cx + x, cy + y, cz + 1.0))
                  (x, y) = (d j, d i)
    let generateRow i = DVector.generate (res * rpp) $ generateCell i
    let expanded = DVector.generate (res * rpp) generateRow
    let addC (Color3 r1 g1 b1) (Color3 r2 g2 b2) = Color3 (r1 + r2) (g1 + g2) (b1 + b2)
    let divC (Color3 r g b) n = let x = fromIntegral n in Color3 (r / x) (g / x) (b / x)
    let reduce vec = DVector.generate res (DVector.generate res . avg)
            where avg i j = divC (foldl1' addC [vec ! (i * rpp + y) ! (j * rpp + x) | x <- [0..rpp - 1], y <- [0..rpp - 1]]) (rpp * rpp) 
    let screen = reduce expanded
    let points = [(i, j) | let s = [0..res - 1], i <- s, j <- s]
    renderPrimitive Points $ forM_ points $ \(i, j) -> do
        let (x, y) = (fromIntegral j / fromIntegral res * 2.0 - 1.0, fromIntegral i / fromIntegral res * 2.0 - 1.0)
        color $ screen ! i ! j
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
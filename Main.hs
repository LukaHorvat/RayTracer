{-# LANGUAGE PatternSynonyms #-}
module Main where

import Graphics.UI.GLUT hiding (GLfloat, initialize, Sphere, Vector3)
import Vector
import Game
import Control.Monad
import Ray
import Scene

red :: Material
red = Material (Color3 1 0 0) 1 (Color3 1 0 0)

none :: Material
none = Material (Color3 1 0 0) 1 (Color3 0.1 0.1 0.1)

scene :: Scene
scene = Scene [ Sphere (Vector3 (-4, 0, 5)) 2 none
              , Sphere (Vector3 (4, 0, 5)) 2 red 
              , Sphere (Vector3 (0, 3, 10)) 2 none ]

render :: () -> IO ()
render _ = do
    let points = [(x / 150.0 - 1.0, y / 150.0 - 1.0) | let s = [0.0..300.0], x <- s, y <- s] 
    renderPrimitive Points $ forM_ points $ \(x, y) -> do
        let ray = Ray (Vector3 (0, 0, 0)) (Vector3 (x, y, 1))
        color $ cast ray scene
        vertex $ Vertex3 x y 0

main :: IO ()
main = run (return ())
           ()
           (const id)
           render 
           0
module Scene where

import Vector
import Graphics.UI.GLUT hiding (Vector3)

data Material = Material 
              { _color :: Color3 GLdouble
              , _diffuse :: GLdouble
              , _glow :: Color3 GLdouble }

data Sphere = Sphere 
            { _spherePos :: Vector3
            , _radius :: GLdouble
            , _sphereMat :: Material }

newtype Scene = Scene [Sphere]
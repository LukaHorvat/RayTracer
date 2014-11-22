module Scene where

import Vector
import Graphics.UI.GLUT hiding (Vector3)

data Material = Material 
              { _color :: Color3 GLfloat
              , _diffuse :: GLfloat
              , _glow :: Color3 GLfloat }

data Sphere = Sphere 
            { _spherePos :: Vector3
            , _radius :: GLfloat
            , _sphereMat :: Material }

newtype Scene = Scene [Sphere]
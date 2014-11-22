module Game where

import Graphics.UI.GLUT hiding (initialize)
import Control.Monad
import Data.IORef
import Data.Function

run :: IO ()
    -> state 
    -> (GLfloat -> state -> state) 
    -> (state -> IO ())
    -> Int
    -> IO ()
run initialize initialState step render mspf = do
    void getArgsAndInitialize
    void $ createWindow "-"
    initialize
    --windowSize $= Size 800 500
    initialDisplayMode $= [DoubleBuffered]
    state <- newIORef initialState
    displayCallback $= do
        clear [ColorBuffer]
        readIORef state >>= render
        swapBuffers
    fix $ \loop -> addTimerCallback mspf $ do
        modifyIORef state (step 0)
        postRedisplay Nothing
        when (mspf /= 0) loop
    mainLoop
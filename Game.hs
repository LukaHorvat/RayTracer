module Game where

import Graphics.UI.GLUT hiding (initialize)
import Control.Monad
import Data.IORef
import Data.Function

run :: IO ()
    -> state 
    -> (GLdouble -> state -> state)
    -> (Key -> KeyState -> Modifiers -> Position -> state -> state)
    -> (state -> IO ())
    -> Int
    -> IO ()
run initialize initialState step input render mspf = do
    void getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered]
    --pointSmooth $= Enabled
    void $ createWindow ""
    initialize
    windowSize $= Size 300 300
    state <- newIORef initialState
    displayCallback $= do
        clear [ColorBuffer]
        readIORef state >>= render
        swapBuffers
    let keyboardHandler key keyState mods pos = do
        modifyIORef state (input key keyState mods pos)
        postRedisplay Nothing
    keyboardMouseCallback $= Just keyboardHandler
    fix $ \loop -> addTimerCallback mspf $ do
        modifyIORef state (step 0)
        postRedisplay Nothing
        when (mspf /= 0) loop
    mainLoop
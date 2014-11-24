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
    actionOnWindowClose $= MainLoopReturns
    --pointSmooth $= Enabled
    _ <- createWindow ""
    initialize
    windowSize $= Size 300 300
    state <- newIORef initialState
    displayCallback $= do
        clear [ColorBuffer]
        readIORef state >>= render
        swapBuffers
        --destroyWindow w
    let keyboardHandler key keyState mods pos = do
        modifyIORef state (input key keyState mods pos)
        postRedisplay Nothing
    keyboardMouseCallback $= Just keyboardHandler
    when (mspf /= 0) $ fix $ \loop -> addTimerCallback mspf $ do
        modifyIORef state (step 0)
        postRedisplay Nothing
        when (mspf /= 0) loop
    mainLoop
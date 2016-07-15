module Main where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  initialWindowSize $= Size 800 600
  initialDisplayMode $= [DoubleBuffered]
  createWindow "Pong?"
  displayCallback $= do
    clear [ColorBuffer]
    swapBuffers
  mainLoop

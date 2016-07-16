module HaskellPong.Initialize where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import HaskellPong.Tick
import HaskellPong.Render
import HaskellPong.Callbacks

pongWindowSize = Size 800 600

initializeWindow :: IO Window
initializeWindow = do
  _ <- getArgsAndInitialize
  initialWindowSize $= pongWindowSize
  initialDisplayMode $= [DoubleBuffered]
  createWindow "Pong?"

initializeGraphics :: IO ()
initializeGraphics = do
  depthMask $= Disabled
  lineSmooth $= Enabled
  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  lineWidth $= 2.0
  viewport $= (Position 0 0, pongWindowSize)
  matrixMode $= Projection
  loadIdentity
  ortho 0 800 600 0 (-1) 1
  matrixMode $= Modelview 0
  loadIdentity
  clearColor $= Color4 0.0 0.0 0.0 1.0

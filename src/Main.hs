module Main where

import HaskellPong.Initialize
import HaskellPong.Callbacks
import Graphics.UI.GLUT (mainLoop)

main :: IO ()
main = do
  initializeWindow
  initializeGraphics
  initializeCallbacks
  mainLoop

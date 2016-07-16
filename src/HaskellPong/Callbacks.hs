module HaskellPong.Callbacks where

import Graphics.UI.GLUT
import HaskellPong.Tick
import HaskellPong.Render
import HaskellPong.GameState (initGameState)

initializeCallbacks :: IO ()
initializeCallbacks = do
  displayCallback $= renderViewport initGameState
  addTimerCallback 0 $ gameTick initGameState

gameTick :: (Renderable t, Tickable t) => t -> IO ()
gameTick t = do
  let newTickable = tick t
  displayCallback $= renderViewport newTickable
  addTimerCallback 33 $ gameTick newTickable
  postRedisplay Nothing

renderViewport :: Renderable r => r -> IO ()
renderViewport r = do
  clear [ColorBuffer]
  render r
  swapBuffers

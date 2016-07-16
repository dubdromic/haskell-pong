module HaskellPong.Callbacks where

import Data.IORef
import Graphics.UI.GLUT
import HaskellPong.Tick
import HaskellPong.Render
import HaskellPong.Keyboard
import HaskellPong.GameState (initGameState)

type KeyboardRef = IORef Keyboard

handleKeyboard :: KeyboardRef -> KeyboardMouseCallback
handleKeyboard kb k ks _ _ = modifyIORef kb (handleKeyEvent k ks)

initializeCallbacks :: IO ()
initializeCallbacks = do
  kb <- newIORef initKeyboard
  keyboardMouseCallback $= Just (handleKeyboard kb)
  displayCallback $= renderViewport initGameState
  addTimerCallback 0 $ gameTick kb initGameState

gameTick :: (Renderable t, Tickable t) => KeyboardRef -> t -> IO ()
gameTick kb t = do
  keys <- readIORef kb
  let newTickable = tick keys t
  displayCallback $= renderViewport newTickable
  addTimerCallback 33 $ gameTick kb newTickable
  postRedisplay Nothing

renderViewport :: Renderable r => r -> IO ()
renderViewport r = do
  clear [ColorBuffer]
  render r
  swapBuffers

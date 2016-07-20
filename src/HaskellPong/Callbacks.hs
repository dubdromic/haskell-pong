module HaskellPong.Callbacks where

import Data.IORef
import Graphics.UI.GLUT
import HaskellPong.Tick
import HaskellPong.Render
import HaskellPong.Keyboard
import HaskellPong.GameState (initGameState)
import System.Random
import System.Exit
import Control.Monad

type KeyboardRef = IORef Keyboard

handleKeyboard :: KeyboardRef -> KeyboardMouseCallback
handleKeyboard kb k ks _ _ = modifyIORef kb (handleKeyEvent k ks)

initializeCallbacks :: IO ()
initializeCallbacks = do
  kb <- newIORef initKeyboard
  xgen <- newStdGen
  let (x, ygen) = randomR (0, ls) xgen
  let (y, _) = randomR (0, ls) ygen
  let vector = (l !! x, l !! y)
  keyboardMouseCallback $= Just (handleKeyboard kb)
  displayCallback $= renderViewport (initGameState vector)
  addTimerCallback 0 $ gameTick kb (initGameState vector)
  where l = [-5.5, -5.0, -4.0, 4.0, 5.0, 5.5]
        ls = length l - 1

gameTick :: (Renderable t, Tickable t) => KeyboardRef -> t -> IO ()
gameTick kb t = do
  keys <- readIORef kb
  handleExit keys
  let newTickable = tick keys t
  displayCallback $= renderViewport newTickable
  addTimerCallback 33 $ gameTick kb newTickable
  postRedisplay Nothing

handleExit :: Keyboard -> IO ()
handleExit kb = when exit exitSuccess -- if exit then exitSuccess else return ()
  where exit = isKeyDown kb keyExit

renderViewport :: Renderable r => r -> IO ()
renderViewport r = do
  clear [ColorBuffer]
  render r
  swapBuffers

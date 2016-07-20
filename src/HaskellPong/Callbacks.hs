module HaskellPong.Callbacks where

import Data.IORef
import Graphics.UI.GLUT
import HaskellPong.Tick
import HaskellPong.Render
import HaskellPong.Keyboard
import HaskellPong.GameState
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
  let vector = randomVector xgen
  keyboardMouseCallback $= Just (handleKeyboard kb)
  displayCallback $= renderViewport (initGameState vector)
  addTimerCallback 0 $ gameTick kb (initGameState vector)

gameTick :: KeyboardRef -> GameState -> IO ()
gameTick kb t = do
  keys <- readIORef kb
  xgen <- newStdGen
  handleExit keys
  let t' = handleRestart keys xgen t
  let newTickable = tick keys t'
  displayCallback $= renderViewport newTickable
  addTimerCallback 33 $ gameTick kb newTickable
  postRedisplay Nothing

handleRestart :: Keyboard -> StdGen -> GameState -> GameState
handleRestart kb gen t = t'
  where t'
          | isKeyDown kb keyr = initGameState $ randomVector gen
          | otherwise = t

handleExit :: Keyboard -> IO ()
handleExit kb = when exit exitSuccess -- if exit then exitSuccess else return ()
  where exit = isKeyDown kb keyq

randomVector :: StdGen -> (Float, Float)
randomVector gen = do
  let (x, gen') = randomR (0, ls) gen
  let (y, _) = randomR (0, ls) gen
  (l !! x, l !! y)
  where l = [-5.5, -5.0, -4.0, 4.0, 5.0, 5.5]
        ls = length l - 1


renderViewport :: Renderable r => r -> IO ()
renderViewport r = do
  clear [ColorBuffer]
  render r
  swapBuffers

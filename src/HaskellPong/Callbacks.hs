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
import Data.Time.Clock.POSIX
import Debug.Trace

type KeyboardRef  = IORef Keyboard
type TimeRef      = IORef POSIXTime
type StateRef     = IORef GameState
data CallbackRefs = CallbackRefs TimeRef TimeRef KeyboardRef StateRef

handleKeyboard :: CallbackRefs -> KeyboardMouseCallback
handleKeyboard refs@(CallbackRefs _ _ kbr _) k ks _ _ = modifyIORef kbr (handleKeyEvent k ks)

initCallbackRefs :: IO CallbackRefs
initCallbackRefs = do
  gen <- newStdGen
  kb <- newIORef initKeyboard
  accum <- newIORef 0
  prev <- getPOSIXTime >>= newIORef
  st <- newIORef $ initGameState $ randomVector gen
  return $ CallbackRefs accum prev kb st

initializeCallbacks :: IO ()
initializeCallbacks = do
  refs <- initCallbackRefs
  keyboardMouseCallback $= Just (handleKeyboard refs)
  displayCallback $= renderViewport refs

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

renderViewport :: CallbackRefs -> IO ()
renderViewport refs@(CallbackRefs ar tr kb rr) = do
  current <- getPOSIXTime
  prev <- readIORef tr
  accum <- readIORef ar
  keys <- readIORef kb
  gs <- readIORef rr
  gen <- newStdGen

  handleExit keys

  let frameTime = min 0.1 $ current - prev
      newAccum = accum + frameTime

  let consumeAccum acc = if acc >= 0.0333
          then do
            modifyIORef rr $ tick keys
            modifyIORef rr $ handleRestart keys gen
            consumeAccum $ acc - 0.0333
          else return acc

  newAccum' <- consumeAccum newAccum

  writeIORef tr current
  writeIORef ar newAccum'

  let interpolation = realToFrac $ newAccum' / 0.0333

  r <- readIORef rr

  clear [ColorBuffer]
  render interpolation r
  swapBuffers
  postRedisplay Nothing

module HaskellPong.Keyboard where

import Data.Set (Set)
import qualified Data.Set as Set
import Graphics.UI.GLUT (Key(..), SpecialKey(..), KeyState(..))

newtype Keyboard = Keyboard (Set Key)

keyUp = SpecialKey KeyUp
keyDown = SpecialKey KeyDown
keyExit = Char 'q'

handleKeyEvent :: Key -> KeyState -> Keyboard -> Keyboard
handleKeyEvent k ks (Keyboard s) = case ks of
  Up   -> Keyboard $ Set.delete k s
  Down -> Keyboard $ Set.insert k s

initKeyboard :: Keyboard
initKeyboard = Keyboard Set.empty

isKeyDown :: Keyboard -> Key -> Bool
isKeyDown (Keyboard s) k = Set.member k s

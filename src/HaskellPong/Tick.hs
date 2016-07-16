module HaskellPong.Tick where

import HaskellPong.Keyboard

class Tickable t where
  tick :: Keyboard -> t -> t

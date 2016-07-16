module HaskellPong.Player where

import HaskellPong.Keyboard
import HaskellPong.Tick
import HaskellPong.Sprite
import HaskellPong.Geometry
import HaskellPong.Render (Renderable(..))

data Player = Player {
  playerPaddle :: Sprite
}

instance Renderable Player where
  triangleVertices (Player s) = map translation playerVertices
    where translation = translateVector $ spritePosition s

instance Tickable Player where
  tick = tickPlayer

tickPlayer :: Keyboard -> Player -> Player
tickPlayer kb (Player s) = Player $ initSprite pos 0 vel
  where pos = spritePosition s
        vel
          | key keyDown = (0, 10)
          | key keyUp = (0, -10)
          | otherwise = (0, 0)
        key = isKeyDown kb

initPlayer :: PVector2 -> Player
initPlayer p = Player $ initSprite p 0 (0,0)

playerVertices :: [PVector2]
playerVertices = [
    (0.0, 0.0),
    (10.0, 0.0),
    (0.0, 50.0),
    (0.0, 50.0),
    (10.0, 50.0),
    (10.0, 0.0)
  ]

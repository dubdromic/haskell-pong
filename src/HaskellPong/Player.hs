module HaskellPong.Player where

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

tickPlayer :: Player -> Player
tickPlayer (Player s) = Player $ Sprite pos 0 vel
  where pos = spritePosition s
        vel = (0, 1)

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

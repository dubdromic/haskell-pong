module HaskellPong.Player where

import HaskellPong.Sprite
import HaskellPong.Geometry
import HaskellPong.Render (Renderable(..))

data Player = Player
  { playerPaddle :: Sprite }

instance Renderable Player where
  triangleVertices (Player s) = map translation playerVertices
    where translation = translateVector $ spritePosition s

initPlayer :: Player
initPlayer = Player $ initSprite(400,300) 0 (0,0)

playerVertices :: [PVector2]
playerVertices = [
    (0.0, 0.0),
    (10.0, 0.0),
    (0.0, 50.0),
    (0.0, 50.0),
    (10.0, 50.0),
    (10.0, 0.0)
  ]

module HaskellPong.Player where

import HaskellPong.Sprite
import HaskellPong.Geometry
import HaskellPong.Render (Renderable(..))

data Player = Player
  { playerPaddle :: Sprite }

instance Renderable Player where
  lineSegments (Player s) = map translation playerLines
    where translation = translateLine $ spritePosition s

initPlayer :: Player
initPlayer = Player $ initSprite(400,300) 0 (0,0)

playerLines :: [PLine]
playerLines = pointsToLines playerPaddlePoints

playerPaddlePoints :: [PVector2]
playerPaddlePoints = [
    (0.0,0.0),
    (10.0, 0.0),
    (10.0, 0.0),
    (10.0, 50.0),
    (10.0, 50.0),
    (0.0, 50.0),
    (0.0, 50.0),
    (0.0, 0.0)
  ]

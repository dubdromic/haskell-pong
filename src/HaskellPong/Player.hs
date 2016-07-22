module HaskellPong.Player where

import HaskellPong.Keyboard
import HaskellPong.Tick
import HaskellPong.Sprite
import HaskellPong.Geometry
import HaskellPong.Collision
import HaskellPong.Render (Renderable(..))

data Player = Player {
  playerPaddle :: Sprite
}

instance Renderable Player where
  vertices i (Player s) = map translation [playerQuad]
    where translation t = translateQuad t $ spritePosition s'
          s' = interpolatedSprite i s

instance Tickable Player where
  tick = tickPlayer

instance Collider Player where
  vertices (Player s) = translateQuad playerQuad $ spritePosition s

collidePaddle :: Player -> Player
collidePaddle = id

tickPlayer :: Keyboard -> Player -> Player
tickPlayer kb (Player s) = Player $ initSprite (px', py') 0 (0, 0) pos
  where pos@(px, py) = spritePosition s
        px' = px
        py'
          | py < 530 && key keyDown = py + 10
          | py > 0 && key keyUp = py - 10
          | otherwise = py
        key = isKeyDown kb
        movable = py >= 0 && py <= 530

initPlayer :: PVector2 -> Player
initPlayer p = Player $ initSprite p 0 (0,0) p

playerQuad :: PQuad
playerQuad = ((0.0, 0.0), (10.0, 0.0), (10.0, 70.0), (0.0, 70.0))

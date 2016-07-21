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
collidePaddle p@(Player ps) = Player $ initSprite pos' 0 vel pos
  where pos@(px, py) = spritePreviousPosition ps
        pos'
          | py <= 0 = (px, 0)
          | py >= 530 = (px, 530)
          | otherwise = (px, py)
        vel = spriteVelocity ps

tickPlayer :: Keyboard -> Player -> Player
tickPlayer kb (Player s) = Player $ initSprite pos 0 vel pos
  where pos = spritePosition s
        vel
          | key keyDown = (0, 10)
          | key keyUp = (0, -10)
          | otherwise = (0, 0)
        key = isKeyDown kb

initPlayer :: PVector2 -> Player
initPlayer p = Player $ initSprite p 0 (0,0) p

playerQuad :: PQuad
playerQuad = ((0.0, 0.0), (10.0, 0.0), (10.0, 70.0), (0.0, 70.0))

module HaskellPong.Ball where

import HaskellPong.Keyboard
import HaskellPong.Tick
import HaskellPong.Sprite
import HaskellPong.Geometry
import HaskellPong.Collision
import HaskellPong.Render (Renderable(..))


data Ball = Ball {
  ballSprite :: Sprite
}

instance Renderable Ball where
  vertices (Ball s) = map translation [ballQuad]
    where translation t = translateQuad t $ spritePosition s

instance Tickable Ball where
  tick = tickBall

instance Collider Ball where
  vertices (Ball s) = translateQuad ballQuad $ spritePosition s

collideBall :: Collider a => [a] -> Ball -> Ball
collideBall a b@(Ball s) = Ball $ initSprite pos 0 vel'
  where collided = any (collides b) a
        pos@(px, py) = spritePosition s
        (vx, vy) = spriteVelocity s
        vel'
          | collided = (-vx, vy)
          | otherwise = spriteVelocity s

tickBall :: Keyboard -> Ball -> Ball
tickBall kb (Ball s) = Ball $ initSprite pos 0 vel
  where pos = spritePosition s
        vel = spriteVelocity s

initBall :: PVector2 -> Ball
initBall p = Ball $ initSprite p 0 (5, 0)

ballQuad :: PQuad
ballQuad = ((0.0, 0.0), (10.0, 0.0), (10.0, 10.0), (0.0, 10.0))

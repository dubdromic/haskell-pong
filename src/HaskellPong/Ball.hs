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
  where paddleCollision = any (collides b) a
        wallCollision = py >= 600 || py <= 0
        pos@(px, py) = spritePosition s
        (vx, vy) = spriteVelocity s
        vel'
          | paddleCollision = (-vx, vy)
          | wallCollision   = (vx, -vy)
          | otherwise       = spriteVelocity s

tickBall :: Keyboard -> Ball -> Ball
tickBall kb (Ball s) = Ball $ initSprite pos 0 vel
  where pos = spritePosition s
        vel = spriteVelocity s

initBall :: PVector2 -> PVector2 -> Ball
initBall p v = Ball $ initSprite p 0 v

ballQuad :: PQuad
ballQuad = ((0.0, 0.0), (10.0, 0.0), (10.0, 10.0), (0.0, 10.0))

module HaskellPong.Ball where

import HaskellPong.Keyboard
import HaskellPong.Tick
import HaskellPong.Sprite
import HaskellPong.Player
import HaskellPong.Geometry
import HaskellPong.Collision
import HaskellPong.Render (Renderable(..))
import Data.List
import Data.Maybe

data Ball = Ball {
  ballSprite :: Sprite
}

instance Renderable Ball where
  vertices i (Ball s) = map translation [ballQuad]
    where translation t = translateQuad t $ spritePosition s'
          s' = interpolatedSprite i s

instance Tickable Ball where
  tick = tickBall

instance Collider Ball where
  vertices (Ball s) = translateQuad ballQuad $ spritePosition s

collideBall :: [Player] -> Ball -> Ball
collideBall a b@(Ball s) = Ball $ initSprite pos 0 vel' pos
  where wallCollision = py >= 590 || py <= 0
        collidedPaddle = find (collides b) a
        pos@(px, py) = spritePreviousPosition s
        (vx, vy) = spriteVelocity s
        vel'
          | isJust collidedPaddle = ballCollidedVelocity b $ fromJust collidedPaddle
          | wallCollision   = (vx, -vy)
          | otherwise       = spriteVelocity s

-- Take a portion of the paddle's Y velocity into account
-- after a ball collision. Also, speed up the ball on every hit.
ballCollidedVelocity :: Ball -> Player -> PVector2
ballCollidedVelocity b@(Ball bs) p@(Player ps) = (vx, vy)
  where vx = -bvx * 1.05
        vy = (bvy + pvym) * 1.05
        (bvx, bvy) = spriteVelocity bs
        pvym = pvy * 0.1
        (_, pvy) = spriteVelocity ps

tickBall :: Keyboard -> Ball -> Ball
tickBall kb (Ball s) = Ball $ initSprite pos 0 vel pos
  where pos = spritePosition s
        vel = spriteVelocity s

initBall :: PVector2 -> PVector2 -> Ball
initBall p v = Ball $ initSprite p 0 v p

ballQuad :: PQuad
ballQuad = ((0.0, 0.0), (10.0, 0.0), (10.0, 10.0), (0.0, 10.0))

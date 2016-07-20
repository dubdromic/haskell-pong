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
  vertices (Ball s) = map translation [ballQuad]
    where translation t = translateQuad t $ spritePosition s

instance Tickable Ball where
  tick = tickBall

instance Collider Ball where
  vertices (Ball s) = translateQuad ballQuad $ spritePosition s

collideBall :: [Player] -> Ball -> Ball
collideBall a b@(Ball s) = Ball $ initSprite pos' 0 vel'
  where wallCollision = py >= 600 || py <= 0
        collidedPaddle = find (collides b) a
        (px, py) = spritePosition s
        (vx, vy) = spriteVelocity s
        pos'
          | isJust collidedPaddle && px > 400 = (770, py)
          | isJust collidedPaddle && px < 400 = (20, py)
          | otherwise = (px, py)
        vel'
          | isJust collidedPaddle = (-vx * 1.01, vy * 1.01) -- ballCollidedVelocity b $ fromJust collidedPaddle
          | wallCollision   = (vx, -vy)
          | otherwise       = spriteVelocity s

-- ballCollidedVelocity :: Ball -> Player -> PVector2
-- ballCollidedVelocity b@(Ball bs) p@(Player ps) = (vx', vy')
--   where vx = if paddleX > 400 then -vx' else vx'
--         vy = -vy'
--         vx' = 5 * cos angle
--         vy' = 5 * sin angle
--         angle = ni * 60
--         ni = riy / 35
--         riy = (paddleY + 35) - (ballY + 5)
--         (paddleX, paddleY) = spritePosition ps
--         (_, ballY) = spritePosition bs


tickBall :: Keyboard -> Ball -> Ball
tickBall kb (Ball s) = Ball $ initSprite pos 0 vel
  where pos = spritePosition s
        vel = spriteVelocity s

initBall :: PVector2 -> PVector2 -> Ball
initBall p v = Ball $ initSprite p 0 v

ballQuad :: PQuad
ballQuad = ((0.0, 0.0), (10.0, 0.0), (10.0, 10.0), (0.0, 10.0))

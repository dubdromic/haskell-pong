module HaskellPong.Ball where

import HaskellPong.Keyboard
import HaskellPong.Tick
import HaskellPong.Sprite
import HaskellPong.Geometry
import HaskellPong.Render (Renderable(..))


data Ball = Ball {
  ballSprite :: Sprite
}

instance Renderable Ball where
  vertices (Ball s) = map translation [ballQuad]
    where translation t = translateQuad t $ spritePosition s

instance Tickable Ball where
  tick = tickBall

ballVelocity :: PVector2
ballVelocity = (5, 5)

tickBall :: Keyboard -> Ball -> Ball
tickBall kb (Ball s) = Ball $ initSprite pos 0 vel
  where pos = spritePosition s
        vel = ballVelocity

initBall :: PVector2 -> Ball
initBall p = Ball $ initSprite p 0 ballVelocity

ballQuad :: PQuad
ballQuad = ((0.0, 0.0), (10.0, 0.0), (10.0, 10.0), (0.0, 10.0))

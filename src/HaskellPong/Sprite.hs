module HaskellPong.Sprite where

import HaskellPong.Geometry

data Sprite = Sprite
  { spritePosition :: PVector2,
    spriteAngle :: Float,
    spriteVelocity :: PVector2
  }

initSprite :: PVector2 -> Float -> PVector2 -> Sprite
initSprite pos ang vel = Sprite (pos /+ vel) ang vel

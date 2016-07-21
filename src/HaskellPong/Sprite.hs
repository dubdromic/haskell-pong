module HaskellPong.Sprite where

import HaskellPong.Geometry

data Sprite = Sprite
  { spritePosition :: PVector2,
    spriteAngle :: Float,
    spriteVelocity :: PVector2,
    spritePreviousPosition :: PVector2
  }

initSprite :: PVector2 -> Float -> PVector2 -> PVector2 -> Sprite
initSprite pos ang vel ppos = Sprite (pos /+ vel) ang vel ppos

interpolatedSprite :: Float -> Sprite -> Sprite
interpolatedSprite i s = s { spritePosition = pos' }
  where pos' = pos /* i /+ (spritePreviousPosition s) /* i'
        pos = spritePosition s
        i' = 1.0 - i

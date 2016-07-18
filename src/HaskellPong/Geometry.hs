module HaskellPong.Geometry where

type PVectorValue = Float
type PVector2 = (PVectorValue, PVectorValue)
type PTriangle = (PVector2, PVector2, PVector2)

translateTriangle :: PTriangle -> PVector2 -> PTriangle
translateTriangle t v = mapPTriangle (/+ v) t
  where mapPTriangle f (a, b, c) = (f a, f b, f c)

(/+) :: PVector2 -> PVector2 -> PVector2
(x, y) /+ (x', y') = (x + x', y + y')

infixl 6 /+

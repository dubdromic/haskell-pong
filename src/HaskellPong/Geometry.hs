module HaskellPong.Geometry where

type PVectorValue = Float
type PVector2 = (PVectorValue, PVectorValue)
type PQuad = (PVector2, PVector2, PVector2, PVector2)

translateQuad :: PQuad -> PVector2 -> PQuad
translateQuad t v = mapPQuad (/+ v) t
  where mapPQuad f (a, b, c, d) = (f a, f b, f c, f d)

(/+) :: PVector2 -> PVector2 -> PVector2
(x, y) /+ (x', y') = (x + x', y + y')

infixl 6 /+

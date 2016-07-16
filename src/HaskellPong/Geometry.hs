module HaskellPong.Geometry where

type PVectorValue = Float
type PVector2 = (PVectorValue, PVectorValue)
newtype PLine = PLine (PVector2, PVector2)

translateVector :: PVector2 -> PVector2 -> PVector2
translateVector(x, y) (x', y') = (x + x', y + y')

(/+) :: PVector2 -> PVector2 -> PVector2
(x, y) /+ (x', y') = (x + x', y + y')

infixl 6 /+

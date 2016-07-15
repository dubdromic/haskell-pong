module HaskellPong.Geometry where

type PVectorValue = Float
type PVector2 = (PVectorValue, PVectorValue)
newtype PLine = PLine (PVector2, PVector2)

pointsToLines :: [PVector2] -> [PLine]
pointsToLines (x:x':[]) = [PLine(x, x')]
pointsToLines (x:x':xs) = PLine(x, x') : pointsToLines xs

translateLine :: PVector2 -> PLine -> PLine
translateLine vector (PLine (x, y)) = PLine(t x, t y)
  where t = translatePoint vector

translatePoint :: PVector2 -> PVector2 -> PVector2
translatePoint (x, y) (x', y') = (x + x', y + y')



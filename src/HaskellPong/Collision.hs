module HaskellPong.Collision where

import HaskellPong.Geometry

class Collider c where
  vertices :: c -> PQuad

  collides :: (Collider d) => c -> d -> Bool
  collides c c' = quadCollision cs cs'
    where cs  = vertices c
          cs' = vertices c'

quadCollision :: PQuad -> PQuad -> Bool
quadCollision c c' = aabb
  where aabb = x < x' + w' &&
               x + w > x'  &&
               y < y' + h' &&
               y + h > y'
        (x, y, w, h) = collisionRelevantPoints c
        (x', y', w', h') = collisionRelevantPoints c'


collisionRelevantPoints :: PQuad -> (PVectorValue, PVectorValue, PVectorValue, PVectorValue)
collisionRelevantPoints ((x1, y1), _, (x2, y2), _) = (x1, y1, x2 - x1, y2 - y1)

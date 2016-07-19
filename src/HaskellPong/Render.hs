module HaskellPong.Render where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import HaskellPong.Geometry

class Renderable r where
  vertices :: r -> [PQuad]

  render :: r -> IO ()
  render = renderVertices . vertices

renderVertices :: [PQuad] -> IO ()
renderVertices quads = do
    currentColor $= Color4 1.0 1.0 1.0 1.0
    renderPrimitive Quads $ mapM_ vectorToVertex verts
  where verts = concatMap toList quads
        toList (a, b, c, d) = [a, b, c, d]

vectorToVertex :: PVector2 -> IO ()
vectorToVertex = vertex . uncurry Vertex2

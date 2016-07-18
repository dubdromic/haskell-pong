module HaskellPong.Render where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import HaskellPong.Geometry

class Renderable r where
  vertices :: r -> [PTriangle]

  render :: r -> IO ()
  render = renderVertices . vertices

renderVertices :: [PTriangle] -> IO ()
renderVertices triangles = do
    currentColor $= Color4 1.0 1.0 1.0 1.0
    renderPrimitive Triangles $ mapM_ vectorToVertex verts
  where verts = concatMap toList triangles
        toList (a, b, c) = [a, b, c]

vectorToVertex :: PVector2 -> IO ()
vectorToVertex = vertex . uncurry Vertex2

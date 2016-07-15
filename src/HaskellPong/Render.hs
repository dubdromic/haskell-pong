module HaskellPong.Render where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import HaskellPong.Geometry

class Renderable r where
  triangleVertices :: r -> [PVector2]

  render :: r -> IO ()
  render = renderVertices . triangleVertices

renderVertices :: [PVector2] -> IO ()
renderVertices verts = do
  currentColor $= Color4 1.0 1.0 1.0 1.0
  renderPrimitive Triangles $ mapM_ vectorToVert verts

vectorToVert :: PVector2 -> IO ()
vectorToVert = vertex . uncurry Vertex2

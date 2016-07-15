module HaskellPong.Render where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import HaskellPong.Geometry

class Renderable r where
  lineSegments :: r -> [PLine]

  render :: r -> IO ()
  render = renderLines . lineSegments

renderLines :: [PLine] -> IO ()
renderLines lines = do
  currentColor $= Color4 1.0 1.0 1.0 1.0
  renderPrimitive Lines $ mapM_ lineVerts lines

lineVerts :: PLine -> IO ()
lineVerts (PLine(x, y)) = pointToVertex x >> pointToVertex y

pointToVertex :: PVector2 -> IO ()
pointToVertex = vertex . uncurry Vertex2

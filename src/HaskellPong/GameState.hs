module HaskellPong.GameState where

import HaskellPong.Geometry
import HaskellPong.Render
import HaskellPong.Player

data GameState = GameState {
  playerOne :: Player,
  playerTwo :: Player
}

instance Renderable GameState where
  triangleVertices = stateVertices

stateVertices :: GameState -> [PVector2]
stateVertices p = pov ++ ptv
  where pov = (triangleVertices . playerOne) p
        ptv = (triangleVertices . playerTwo) p

initGameState :: GameState
initGameState = GameState {
  playerOne = initPlayer(10, 10),
  playerTwo = initPlayer(780, 10)
}

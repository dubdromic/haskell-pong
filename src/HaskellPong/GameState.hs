module HaskellPong.GameState where

import HaskellPong.Keyboard
import HaskellPong.Geometry
import HaskellPong.Render
import HaskellPong.Player
import HaskellPong.Tick

data GameState = GameState {
  playerOne :: Player,
  playerTwo :: Player
}

instance Renderable GameState where
  vertices = stateVertices

instance Tickable GameState where
  tick = tickState

stateVertices :: GameState -> [PTriangle]
stateVertices p = pov ++ ptv
  where pov = (vertices . playerOne) p
        ptv = (vertices . playerTwo) p

tickState :: Keyboard -> GameState -> GameState
tickState kb (GameState p1 p2) = GameState p1' p2'
  where p1' = tick kb p1
        p2' = tick kb p2

initGameState :: GameState
initGameState = GameState {
  playerOne = initPlayer(10, 280),
  playerTwo = initPlayer(780, 280)
}

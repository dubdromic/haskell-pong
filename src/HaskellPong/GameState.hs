module HaskellPong.GameState where

import HaskellPong.Keyboard
import HaskellPong.Geometry
import HaskellPong.Render
import HaskellPong.Player
import HaskellPong.Ball
import HaskellPong.Tick

data GameState = GameState {
  gameStatePlayerOne :: Player,
  gameStatePlayerTwo :: Player,
  gameStateBall :: Ball
}

instance Renderable GameState where
  vertices = stateVertices

instance Tickable GameState where
  tick = tickState

stateVertices :: GameState -> [PTriangle]
stateVertices p = pov ++ ptv ++ bv
  where pov = (vertices . gameStatePlayerOne) p
        ptv = (vertices . gameStatePlayerTwo) p
        bv  = (vertices . gameStateBall) p

tickState :: Keyboard -> GameState -> GameState
tickState kb (GameState p1 p2 b) = GameState p1' p2' b'
  where p1' = tick kb p1
        p2' = tick kb p2
        b'  = tick kb b

initGameState :: GameState
initGameState = GameState {
  gameStatePlayerOne = initPlayer(10, 280),
  gameStatePlayerTwo = initPlayer(780, 280),
  gameStateBall = initBall(395, 295)
}

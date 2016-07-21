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

stateVertices :: Float -> GameState -> [PQuad]
stateVertices i p = pov ++ ptv ++ bv
  where pov = (vertices i . gameStatePlayerOne) p
        ptv = (vertices i . gameStatePlayerTwo) p
        bv  = (vertices i . gameStateBall) p

tickState :: Keyboard -> GameState -> GameState
tickState kb (GameState p1 p2 b) = GameState p1' p2' b'
  where p1' = collidePaddle $ tick kb p1
        p2' = collidePaddle $ tick kb p2
        b'  = collideBall [p1', p2'] $ tick kb b

initGameState :: PVector2 -> GameState
initGameState a = GameState {
  gameStatePlayerOne = initPlayer(10, 280),
  gameStatePlayerTwo = initPlayer(780, 280),
  gameStateBall = initBall (395, 295) a
}

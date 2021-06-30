module Game.GameState where

import qualified Data.Vector as V
import Game.BoardGeneration
import Game.UnitPlacement
import qualified System.Random as R

data GameState = GameState
  { board :: Board Tile,
    units :: V.Vector Unit,
    turnCount :: Int,
    randomGen :: R.StdGen
  }

-- ~ Rough print of the game state used for debugging purposes.
printGameState :: GameState -> IO ()
printGameState gameState = do
  printUnits (units gameState)
  putStrLn ("Turn Count: " ++ show (turnCount gameState))
  putStrLn ""

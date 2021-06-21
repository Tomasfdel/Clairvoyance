module GameState where

import BoardGeneration
import qualified Data.Vector as V
import qualified System.Random as R
import UnitPlacement

data GameState = GameState
  { board :: Board Tile,
    units :: V.Vector Unit,
    turnCount :: Int,
    randomGen :: R.StdGen
  }

-- ~ Rough print of the game state used for debugging purposes.
printGameState :: GameState -> IO ()
printGameState gameState = do
  printBoard (board gameState)
  printUnits (units gameState)
  putStrLn ("Turn Count: " ++ show (turnCount gameState))
  putStrLn ""

module GameState where

import BoardGeneration
import UnitPlacement
import qualified Data.Vector as V
import qualified System.Random as R

data GameState = GameState { board :: Board,
                             units :: V.Vector Unit,
                             turnCount :: Int,
                             randomGen :: R.StdGen }

module BreadthFirstSearch where

import BoardGeneration
import Control.Monad.State
import qualified Data.PSQueue as PQ
import qualified Data.Sequence as Seq
import qualified Data.Vector as V
import GameState
import ParserTypes
import UnitPlacement

type DistanceMapTile = (Coordinate, Float)

tileDistance :: DistanceMapTile -> Int
tileDistance (_, distance) = floor distance

-- Checks if a tile was previously visited. That is, if its distance value
-- is set or if it's still the initial value -1.
isTileVisited :: DistanceMapTile -> Bool
isTileVisited tile = (tileDistance tile) >= 0

isUnitTile :: Tile -> Bool
isUnitTile (Unit _) = True
isUnitTile _ = False

isValidStraightMovement :: Board Tile -> Coordinate -> Coordinate -> Bool
isValidStraightMovement board (startCol, startRow) (endCol, endRow) =
  ( startCol == endCol && abs (startRow - endRow) == 1
      || startRow == endRow && abs (startCol - endCol) == 1
  )
    && (board V.! endRow) V.! endCol == Empty

isValidDiagonalMovement :: Board Tile -> Coordinate -> Coordinate -> Bool
isValidDiagonalMovement board (startCol, startRow) (endCol, endRow) =
  abs (startCol - endCol) == 1 && abs (startRow - endRow) == 1
    && (board V.! endRow) V.! endCol == Empty
    && (board V.! startRow) V.! endCol == Empty
    && (board V.! endRow) V.! startCol == Empty

isValidMovement :: Board Tile -> Coordinate -> Coordinate -> Bool
isValidMovement board start end =
  validCoord board start && validCoord board end
    && (isValidStraightMovement board start end || isValidDiagonalMovement board start end)

-- ~ Checks that its possible to move to the given tile. To do so, the distance of the
-- ~ tile to the center of the search has to be at most the max range, has to be inside
-- ~ the map, not be already visited and not be a wall.
findSideTile :: Board Tile -> Bool -> Coordinate -> Int -> Int -> State (Board DistanceMapTile, [(Coordinate, Float)]) ()
findSideTile board ignoreObjects (col, row) colDiff rowDiff = state $ \(visited, found) ->
  if validCoord board (col + colDiff, row + rowDiff)
    && not (isTileVisited ((visited V.! (row + rowDiff)) V.! (col + colDiff)))
    then
      let sideTile = (board V.! (row + rowDiff)) V.! (col + colDiff)
          (_, currentDistance) = (visited V.! row) V.! col
          newDistanceTile = ((col, row), currentDistance + 1)
          newVisRow = (visited V.! (row + rowDiff)) V.// [(col + colDiff, newDistanceTile)]
          newVisited = visited V.// [(row + rowDiff, newVisRow)]
       in if ignoreObjects
            || not (isUnitTile sideTile) && sideTile /= Wall
            then ((), (newVisited, ((col + colDiff, row + rowDiff), currentDistance + 1) : found))
            else ((), (newVisited, found))
    else ((), (visited, found))

-- ~ Does the same check as findSideTile, with two differences: the distance is 1.5
-- ~ instead of 1, since this is a diagonal move, and both the horizontal and vertical
-- ~ adjacent tiles have to be visitable in order to be able to move diagonally.
findDiagonalTile :: Board Tile -> Bool -> Coordinate -> Int -> Int -> State (Board DistanceMapTile, [(Coordinate, Float)]) ()
findDiagonalTile board ignoreObjects (col, row) colDiff rowDiff = state $ \(visited, found) ->
  if validCoord board (col + colDiff, row + rowDiff)
    && not (isTileVisited ((visited V.! (row + rowDiff)) V.! (col + colDiff)))
    && ( ignoreObjects
           || (board V.! (row + rowDiff)) V.! (col + colDiff) == Empty
             && (board V.! row) V.! (col + colDiff) == Empty
             && (board V.! (row + rowDiff)) V.! col == Empty
           || isUnitTile ((board V.! (row + rowDiff)) V.! (col + colDiff))
             && ((board V.! row) V.! (col + colDiff) /= Wall || (board V.! (row + rowDiff)) V.! col /= Wall)
       )
    then
      let diagonalTile = (board V.! (row + rowDiff)) V.! (col + colDiff)
          (_, currentDistance) = (visited V.! row) V.! col
          newDistanceTile = ((col, row), currentDistance + 1.5)
          newVisRow = (visited V.! (row + rowDiff)) V.// [(col + colDiff, newDistanceTile)]
          newVisited = visited V.// [(row + rowDiff, newVisRow)]
       in if ignoreObjects || not (isUnitTile diagonalTile)
            then ((), (newVisited, ((col + colDiff, row + rowDiff), currentDistance + 1.5) : found))
            else ((), (newVisited, found))
    else ((), (visited, found))

-- ~ Returns a list of the adjacent tiles that should be explored and updates
-- ~ the board of visited tiles.
findNewTiles :: Board Tile -> Bool -> Coordinate -> State (Board DistanceMapTile, [(Coordinate, Float)]) ()
findNewTiles board ignoreObjects pos = do
  findSideTile board ignoreObjects pos 0 1
  findSideTile board ignoreObjects pos 0 (-1)
  findSideTile board ignoreObjects pos 1 0
  findSideTile board ignoreObjects pos (-1) 0
  findDiagonalTile board ignoreObjects pos 1 1
  findDiagonalTile board ignoreObjects pos 1 (-1)
  findDiagonalTile board ignoreObjects pos (-1) 1
  findDiagonalTile board ignoreObjects pos (-1) (-1)
  return ()

setSearchStartTiles :: Board DistanceMapTile -> [Coordinate] -> Board DistanceMapTile
setSearchStartTiles distanceMap [] = distanceMap
setSearchStartTiles distanceMap ((col, row) : cs) =
  let searchStartTile = ((-1, -1), 0)
      updatedRow = (distanceMap V.! row) V.// [(col, searchStartTile)]
      updatedMap = distanceMap V.// [(row, updatedRow)]
   in setSearchStartTiles updatedMap cs

-- ~ Creates a board with the same dimensions as the given one, but full of -1 values,
-- ~ except in the given position where there is a 0.
createSearchBoard :: Board Tile -> [Coordinate] -> Board DistanceMapTile
createSearchBoard board startCoords =
  let height = V.length board
      width = V.length (V.head board)
      emptyDistanceTile = ((-1, -1), -1)
      baseBoard = V.replicate height (V.replicate width emptyDistanceTile)
   in setSearchStartTiles baseBoard startCoords

buildPathToTarget :: Coordinate -> Board DistanceMapTile -> [(Coordinate, Int)]
buildPathToTarget (col, row) markedMap =
  let (previousCoordinate, currentDistance) = (markedMap V.! row) V.! col
   in if currentDistance == 0
        then [((col, row), floor currentDistance)]
        else ((col, row), floor currentDistance) : (buildPathToTarget previousCoordinate markedMap)

insertTilesInQueue :: PQ.PSQ Coordinate Float -> [(Coordinate, Float)] -> PQ.PSQ Coordinate Float
insertTilesInQueue queue [] = queue
insertTilesInQueue queue ((coord, distance) : xs) = insertTilesInQueue (PQ.insert coord distance queue) xs

expandTiles :: Board Tile -> Board DistanceMapTile -> Bool -> PQ.PSQ Coordinate Float -> Board DistanceMapTile
-- TO DO: This looks like it can be expanded into a state function.
expandTiles board distanceMap ignoreObjects queue =
  case PQ.minView queue of
    Nothing -> distanceMap
    Just (binding, newQueue) ->
      let ((), (newMap, newTiles)) = runState (findNewTiles board ignoreObjects (PQ.key binding)) (distanceMap, [])
       in expandTiles board newMap ignoreObjects (insertTilesInQueue newQueue newTiles)

buildWalkingDistanceMap :: Board Tile -> [Coordinate] -> Board DistanceMapTile
buildWalkingDistanceMap board startCoords = expandTiles board (createSearchBoard board startCoords) False (PQ.fromList (map (\coord -> coord PQ.:-> 0) startCoords))

buildFloatingDistanceMap :: Board Tile -> [Coordinate] -> Board DistanceMapTile
buildFloatingDistanceMap board startCoords = expandTiles board (createSearchBoard board startCoords) True (PQ.fromList (map (\coord -> coord PQ.:-> 0) startCoords))

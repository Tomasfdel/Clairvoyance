module BreadthFirstSearch where

import BoardGeneration
import Control.Monad.State
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

-- ~ Checks that its possible to move to the given tile. To do so, the distance of the
-- ~ tile to the center of the search has to be at most the max range, has to be inside
-- ~ the map, not be already visited and not be a wall.
findSideTile :: Board Tile -> Bool -> Coordinate -> Int -> Int -> State (Board DistanceMapTile, [Coordinate]) ()
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
            then ((), (newVisited, (col + colDiff, row + rowDiff) : found))
            else ((), (newVisited, found))
    else ((), (visited, found))

-- ~ Does the same check as findSideTile, with two differences: the distance is 1.5
-- ~ instead of 1, since this is a diagonal move, and both the horizontal and vertical
-- ~ adjacent tiles have to be visitable in order to be able to move diagonally.
findDiagonalTile :: Board Tile -> Bool -> Coordinate -> Int -> Int -> State (Board DistanceMapTile, [Coordinate]) ()
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
            then ((), (newVisited, (col + colDiff, row + rowDiff) : found))
            else ((), (newVisited, found))
    else ((), (visited, found))

-- ~ Returns a list of the adjacent tiles that should be explored and updates
-- ~ the board of visited tiles.
findNewTiles :: Board Tile -> Bool -> Coordinate -> State (Board DistanceMapTile, [Coordinate]) ()
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

-- ~ Creates a board with the same dimensions as the given one, but full of -1 values,
-- ~ except in the given position where there is a 0.
createSearchBoard :: Board Tile -> Coordinate -> Board DistanceMapTile
createSearchBoard board (startCol, startRow) =
  let height = V.length board
      width = V.length (V.head board)
      baseDistanceTile = ((-1, -1), 0)
      emptyDistanceTile = ((-1, -1), -1)
      baseBoard = V.replicate height (V.replicate width emptyDistanceTile)
      updatedRow = (baseBoard V.! startRow) V.// [(startCol, baseDistanceTile)]
      updatedBoard = baseBoard V.// [(startRow, updatedRow)]
   in updatedBoard

buildPathToTarget :: Coordinate -> Board DistanceMapTile -> [(Coordinate, Int)]
buildPathToTarget (col, row) markedMap =
  let (previousCoordinate, currentDistance) = (markedMap V.! row) V.! col
   in if currentDistance == 0
        then [((col, row), floor currentDistance)]
        else ((col, row), floor currentDistance) : (buildPathToTarget previousCoordinate markedMap)

expandTiles :: Board Tile -> Board DistanceMapTile -> Bool -> Seq.Seq Coordinate -> Board DistanceMapTile
-- TO DO: This looks like it can be expanded into a state function.
expandTiles board distanceMap ignoreObjects queue =
  if null queue
    then distanceMap
    else
      let ((), (newMap, reversedCoords)) = runState (findNewTiles board ignoreObjects (Seq.index queue 0)) (distanceMap, [])
       in expandTiles board newMap ignoreObjects ((Seq.drop 1 queue) Seq.>< (Seq.fromList (reverse reversedCoords)))

buildWalkingDistanceMap :: Board Tile -> Coordinate -> Board DistanceMapTile
buildWalkingDistanceMap board startCoord = expandTiles board (createSearchBoard board startCoord) False (Seq.singleton startCoord)

buildFloatingDistanceMap :: Board Tile -> Coordinate -> Board DistanceMapTile
buildFloatingDistanceMap board startCoord = expandTiles board (createSearchBoard board startCoord) True (Seq.singleton startCoord)

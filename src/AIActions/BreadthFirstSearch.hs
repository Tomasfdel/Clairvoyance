module AIActions.BreadthFirstSearch where

import Control.Monad.State
import qualified Data.PSQueue as PQ
import qualified Data.Sequence as Seq
import qualified Data.Vector as V
import FileParser.Types
import Game.BoardGeneration
import Game.GameState
import Game.UnitPlacement

type DistanceMapTile = (Coordinate, Float)

-- ~ Returns the distance of the given distance map tile, rounded down.
tileDistance :: DistanceMapTile -> Int
tileDistance (_, distance) = floor distance

-- Checks if a tile was previously visited. That is, if its distance value
-- is set or if it's still the initial value -1.
isTileVisited :: DistanceMapTile -> Bool
isTileVisited tile = (tileDistance tile) >= 0

-- ~ Checks if there's a unit in the given tile.
isUnitTile :: Tile -> Bool
isUnitTile (Unit _) = True
isUnitTile _ = False

-- ~ Returns the distance to a given unit in the distance map.
getUnitDistance :: Board DistanceMapTile -> Unit -> Int
getUnitDistance distanceMap unit =
  let (col, row) = getPosition unit
   in floor (snd ((distanceMap V.! row) V.! col))

-- ~ Checks if there's a path towards the given unit in the distance map.
isUnitReachable :: Board DistanceMapTile -> Unit -> Bool
isUnitReachable distanceMap unit = (getUnitDistance distanceMap unit) >= 0

-- ~ Checks that both tiles are adjacent vertically or horizontally and that the end
-- ~ is an empty tile.
isValidStraightMovement :: Board Tile -> Coordinate -> Coordinate -> Bool
isValidStraightMovement board (startCol, startRow) (endCol, endRow) =
  ( startCol == endCol && abs (startRow - endRow) == 1
      || startRow == endRow && abs (startCol - endCol) == 1
  )
    && (board V.! endRow) V.! endCol == Empty

-- ~ Checks both coordinates are adjacent diagonally and that both the end
-- ~ and both tiles adjacent to the movement are empty.
isValidDiagonalMovement :: Board Tile -> Coordinate -> Coordinate -> Bool
isValidDiagonalMovement board (startCol, startRow) (endCol, endRow) =
  abs (startCol - endCol) == 1 && abs (startRow - endRow) == 1
    && (board V.! endRow) V.! endCol == Empty
    && (board V.! startRow) V.! endCol == Empty
    && (board V.! endRow) V.! startCol == Empty

-- ~ Checks that both coordinates are valid and that it's possible to move from
-- ~ the start to the end in one step.
isValidMovement :: Board Tile -> Coordinate -> Coordinate -> Bool
isValidMovement board start end =
  validCoord board start && validCoord board end
    && (isValidStraightMovement board start end || isValidDiagonalMovement board start end)

-- ~ Checks that its possible to move to the given tile. To do so, it has to be inside
-- ~ the map and not be already visited.
-- ~ In case it's a wall or a unit and the search cannot move through objects, the tile
-- ~ is marked as visited but will not be expanded. If not, it will be marked visited
-- ~ and added to the list of tiles to expand further in the search.
findSideTile :: Board Tile -> Bool -> Coordinate -> Int -> Int -> State (Board DistanceMapTile, [(Coordinate, Float)]) ()
findSideTile board ignoreObjects (col, row) colDiff rowDiff = state $ \(visited, found) ->
  if validCoord board (col + colDiff, row + rowDiff)
    && not (isTileVisited ((visited V.! (row + rowDiff)) V.! (col + colDiff)))
    then
      let sideTile = (board V.! (row + rowDiff)) V.! (col + colDiff)
          (_, currentDistance) = (visited V.! row) V.! col
          newVisited = updateBoard visited (col + colDiff, row + rowDiff) ((col, row), currentDistance + 1)
       in if ignoreObjects
            || not (isUnitTile sideTile) && sideTile /= Wall
            then ((), (newVisited, ((col + colDiff, row + rowDiff), currentDistance + 1) : found))
            else ((), (newVisited, found))
    else ((), (visited, found))

-- ~ Does the same check as findSideTile, with two differences: the distance is 1.5
-- ~ instead of 1, and the diagonal move must be valid according to movement rules.
-- ~ The tile will be marked as explored if the search can move through objects, if both
-- ~ it and the two adjacent tiles in the movement are empty or if it's a unit and at least
-- ~ one of the adjacent tiles is not a wall. In all those cases, the diagonal tile is reachable
-- ~ from the origin of the movement.
-- ~ However, the tile will only be expanded if the search ignores objects or if
-- ~ the diagonal tile is empty, meaning that the unit can move into that tile.
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
          newVisited = updateBoard visited (col + colDiff, row + rowDiff) ((col, row), currentDistance + 1.5)
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

-- ~ Sets the search tiles of the distance map as explored with a distance of 0
-- ~ in all the given list's coordinates.
setSearchStartTiles :: Board DistanceMapTile -> [Coordinate] -> Board DistanceMapTile
setSearchStartTiles distanceMap [] = distanceMap
setSearchStartTiles distanceMap ((col, row) : cs) =
  setSearchStartTiles (updateBoard distanceMap (col, row) ((-1, -1), 0)) cs

-- ~ Creates a board with the same dimensions as the given one, but full of
-- ~ unexplored tiles, except in the given source coordinates, where there
-- ~ is a distance 0.
createSearchBoard :: Board Tile -> [Coordinate] -> Board DistanceMapTile
createSearchBoard board startCoords =
  let height = V.length board
      width = V.length (V.head board)
      emptyDistanceTile = ((-1, -1), -1)
      baseBoard = V.replicate height (V.replicate width emptyDistanceTile)
   in setSearchStartTiles baseBoard startCoords

-- ~ Builds a path from the given coordinate to its closest source in the distance map.
buildPathToTarget :: Coordinate -> Board DistanceMapTile -> [(Coordinate, Int)]
buildPathToTarget (col, row) markedMap =
  let (previousCoordinate, currentDistance) = (markedMap V.! row) V.! col
   in if currentDistance == 0
        then [((col, row), floor currentDistance)]
        else ((col, row), floor currentDistance) : (buildPathToTarget previousCoordinate markedMap)

-- ~ Inserts a list of pairs into the tile priority queue used in the expansion algorithm.
insertTilesInQueue :: PQ.PSQ Coordinate Float -> [(Coordinate, Float)] -> PQ.PSQ Coordinate Float
insertTilesInQueue queue [] = queue
insertTilesInQueue queue ((coord, distance) : xs) = insertTilesInQueue (PQ.insert coord distance queue) xs

-- ~ Builds a distance map by keeping a priority queue (ordered by distance to its closest source)
-- ~ of the tiles that should be expanded looking for unexplored tiles.
expandTiles :: Board Tile -> Board DistanceMapTile -> Bool -> PQ.PSQ Coordinate Float -> Board DistanceMapTile
expandTiles board distanceMap ignoreObjects queue =
  case PQ.minView queue of
    Nothing -> distanceMap
    Just (binding, newQueue) ->
      let ((), (newMap, newTiles)) = runState (findNewTiles board ignoreObjects (PQ.key binding)) (distanceMap, [])
       in expandTiles board newMap ignoreObjects (insertTilesInQueue newQueue newTiles)

-- ~ Builds a distance map using the given coordinates as sources
-- ~ without allowing the paths to move through units or walls.
buildWalkingDistanceMap :: Board Tile -> [Coordinate] -> Board DistanceMapTile
buildWalkingDistanceMap board startCoords = expandTiles board (createSearchBoard board startCoords) False (PQ.fromList (map (\coord -> coord PQ.:-> 0) startCoords))

-- ~ Builds a distance map using the given coordinates as sources
-- ~ allowing the paths to move through units or walls.
buildFloatingDistanceMap :: Board Tile -> [Coordinate] -> Board DistanceMapTile
buildFloatingDistanceMap board startCoords = expandTiles board (createSearchBoard board startCoords) True (PQ.fromList (map (\coord -> coord PQ.:-> 0) startCoords))

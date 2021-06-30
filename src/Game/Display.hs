module Game.Display where

import qualified Data.Vector as V
import Game.BoardGeneration

-- ~ Prints the given indices.
printInitiativeOrder :: V.Vector Int -> IO ()
printInitiativeOrder indices = do
  putStrLn "Initiative order:"
  putStrLn (V.foldl1 (\a b -> a ++ ", " ++ b) (V.map show indices))
  putStrLn ""

-- ~ String for an empty tile row.
emptyTileSpace :: String
emptyTileSpace = "    "

-- ~ String for a wall tile row.
wallTileSpace :: String
wallTileSpace = "||||"

-- ~ String for a horizontal board line around a tile.
edgeTileSpace :: String
edgeTileSpace = "----"

-- ~ Converts the index to a string centered with a couple spaces.
showUnitIndex :: Int -> String
showUnitIndex index = " " ++ show (mod (div index 10) 10) ++ show (mod index 10) ++ " "

-- ~ Returns the string corresponding to the top or bottom row of a tile's printed version.
showOuterTileRow :: Tile -> String
showOuterTileRow Wall = wallTileSpace
showOuterTileRow _ = emptyTileSpace

-- ~ Returns the string corresponding to the middle row of a tile's printed version.
showMiddleTileRow :: Tile -> String
showMiddleTileRow Empty = emptyTileSpace
showMiddleTileRow Wall = wallTileSpace
showMiddleTileRow (Unit index) = showUnitIndex index

-- ~ Concatenates the vector of string while interspersing pipe characters.
joinTileRows :: V.Vector String -> String
joinTileRows rows = "|" ++ (V.foldr (\a b -> a ++ "|" ++ b) "\n" rows)

-- ~ Returns a string for the given board row.
showBoardRow :: V.Vector Tile -> String
showBoardRow row = joinTileRows (V.map showMiddleTileRow row)

-- ~ String for the top and bottom horizontal lines of a board of the given width.
boardOuterEdge :: Int -> String
boardOuterEdge width = (replicate (4 * width + width + 1) '-') ++ "\n"

-- ~ String for the inner horizontal lines of a board of the given width.
boardInnerEdge :: Int -> String
boardInnerEdge width = "|" ++ foldr (\a b -> a ++ "|" ++ b) "\n" (replicate width edgeTileSpace)

-- ~ Pretty prints the given board.
printBoard :: Board Tile -> IO ()
printBoard board =
  let width = V.length (V.head board)
      outerEdge = boardOuterEdge width
      innerEdge = boardInnerEdge width
      boardRows = V.map showBoardRow board
      boardString = outerEdge ++ (V.foldr1 (\a b -> a ++ innerEdge ++ b) boardRows) ++ outerEdge
   in putStrLn boardString

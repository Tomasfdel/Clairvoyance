module Game.Display where

import qualified Data.Vector as V
import FileParser.Types
import Game.BoardGeneration
import Game.StatBlockGeneration
import Game.UnitPlacement

-- ~ Prints the given indices.
printInitiativeOrder :: V.Vector Int -> IO ()
printInitiativeOrder indices = do
  putStrLn "Initiative order:"
  putStrLn (V.foldl1 (\a b -> a ++ ", " ++ b) (V.map show indices))
  putStrLn ""

-- ~ Formats an attack range.
showRange :: AttackRange -> String
showRange Melee = "Melee"
showRange (Ranged range) = "Ranged(" ++ show range ++ ")"

-- ~ Formats a given modifier value.
showModifier :: Int -> String
showModifier mod
  | mod > 0 = "+" ++ show mod
  | mod == 0 = ""
  | mod < 0 = show mod

-- ~ Converts the dice roll to its notation.
showDieRoll :: DieRoll -> String
showDieRoll dieRoll = show (dieAmount dieRoll) ++ "d" ++ show (dieValue dieRoll) ++ showModifier (modifier dieRoll)

-- ~ Converts the attack description to a string.
showAttack :: AttackDesc -> String
showAttack (range, mod, damage) = showRange range ++ " " ++ showModifier mod ++ " " ++ showDieRoll damage

-- ~ Returns a string describing the attack list of a unit.
showAttackList :: [AttackDesc] -> String
showAttackList attacks = foldl1 (\a b -> a ++ ", " ++ b) (map showAttack attacks)

-- ~ Pretty prints all relevant statistics of the given unit.
printUnit :: Int -> Unit -> IO ()
printUnit index (Mob unit) =
  let stats = statBlock unit
   in do
        putStrLn ((team unit) ++ " : " ++ (name unit) ++ " : " ++ (show (identifier unit)) ++ "  (Index " ++ (show index) ++ ")")
        putStrLn "AI-controlled unit"
        putStrLn ("  Status: " ++ if unitIsAlive (Mob unit) then "Alive" else "Dead")
        putStrLn ("  Position: " ++ show (position unit))
        putStrLn ("  Health Points: " ++ show (healthPoints stats) ++ " / " ++ show (maxHealthPoints stats))
        putStrLn ("  Speed: " ++ show (speed stats))
        putStrLn ("  Armor Class: " ++ show (armorClass stats))
        putStrLn ("  Attack: " ++ showAttackList (attack stats))
        putStrLn ("  Full Attack : " ++ showAttackList (fullAttack stats))
printUnit index (Player unit) = do
  putStrLn ((playerTeam unit) ++ " : " ++ (playerName unit) ++ " : " ++ (show (playerIdentifier unit)) ++ "  (Index " ++ (show index) ++ ")")
  putStrLn "Player-controlled unit"
  putStrLn ("  Status: " ++ if unitIsAlive (Player unit) then "Alive" else "Dead")
  putStrLn ("  Position: " ++ show (playerPosition unit))

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

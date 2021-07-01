module Game.PlayGame where

import AIActions.Evaluate
import Auxiliary.State
import Commands.Handler
import Control.Monad.State
import qualified Data.List as L
import qualified Data.Vector as V
import FileParser.Lexer
import FileParser.Parser
import Game.BoardGeneration
import Game.Display
import Game.GameState
import Game.StatBlockGeneration
import Game.UnitPlacement
import qualified System.Random as R

-- ~ Rolls initiative and adds the given modifier.
initiativeDieRoll :: (Int, Int) -> IO (Int, Int)
initiativeDieRoll (index, mod) = do
  result <- R.randomRIO (1, 20)
  return (index, result + mod)

-- ~ Returns a pair of unit index and initiative roll for each unit.
initiativeDiceRolls :: V.Vector Unit -> [Int] -> IO [(Int, Int)]
initiativeDiceRolls units indices =
  let indicesWithMods = map (\i -> (i, getInitiative (units V.! i))) indices
   in mapM initiativeDieRoll indicesWithMods

-- ~ Orders the unit indices in descending order of their initiative rolls,
-- ~ grouping those with the same roll in a list.
regroupInitiatives :: [(Int, Int)] -> [[Int]]
regroupInitiatives rolls =
  let reorder = (reverse (L.sortOn snd rolls))
      tupleGroups = L.groupBy (\x y -> (snd x) == (snd y)) reorder
   in map (map fst) tupleGroups

-- ~ Reorders unit indices in descending order of their initiative rolls,
-- ~ breaking ties with another roll among tied units.
initiativeReorder :: V.Vector Unit -> [Int] -> IO [Int]
initiativeReorder _ [] = return []
initiativeReorder _ [n] = return [n]
initiativeReorder units indices = do
  results <- initiativeDiceRolls units indices
  reorderedGroups <- mapM (initiativeReorder units) (regroupInitiatives results)
  return (concat reorderedGroups)

-- ~ Returns a list of the unit indices, ordered by their initiative rolls.
initiativeRoll :: V.Vector Unit -> IO (V.Vector Int)
initiativeRoll units = do
  initList <- initiativeReorder units [0 .. (V.length units - 1)]
  return (V.fromList initList)

-- ~ Takes the turn of the unit with the given index.
-- ~ After each unit's turn, it sets up a console to allow the players to input commands.
takeTurn :: Int -> StateT GameState IO ()
takeTurn index = do
  gameState <- get
  if unitIsAlive ((units gameState) V.! index)
    then do
      lift $ putStrLn ""
      lift $ putStrLn "Current turn: "
      lift $ putStrLn (showCompleteUnitName ((units gameState) V.! index) ++ "  (Index " ++ show index ++ ")")
      lift $ putStrLn ""
      case (units gameState) V.! index of
        Mob unit -> do
          (newAI, _) <- aiStep index (getAI ((units gameState) V.! index))
          morphStateFunction $ updateUnitAI index newAI
        Player player -> return ()
      lift $ putStrLn ""
      commandInput
    else return ()

-- ~ Determines which unit should take its turn, and modifies the game state after it.
turnHandler :: V.Vector Int -> Int -> StateT GameState IO ()
turnHandler initiative index = do
  takeTurn (initiative V.! index)
  gameState <- get
  let newIndex = mod (index + 1) (V.length initiative)
      newTurn = if newIndex == 0 then turnCount gameState + 1 else turnCount gameState
   in do
        modify (\gameState -> gameState {turnCount = newTurn})
        turnHandler initiative newIndex

-- ~ Sets the initiative order for units and starts the first turn.
playGame :: Board Tile -> V.Vector Unit -> IO ()
playGame board units = do
  init <- initiativeRoll units
  printInitiativeOrder init
  randomGen <- R.getStdGen
  let gameState = GameState {board = board, units = units, turnCount = 1, randomGen = randomGen}
   in do
        printBoard board
        newState <- execStateT commandInput gameState
        evalStateT (turnHandler init 0) newState

-- ~ Parses the input file contents and creates all the necessary objects
-- ~ to handle the game state.
setupGame :: String -> Either String (Board Tile, V.Vector Unit)
setupGame input =
  let (boardIn, unitIn, aiIn, teamIn) = parse (alexScanTokens input)
   in do
        (board, offset) <- convertBoardInput boardIn
        units <- convertStatInputs unitIn
        ais <- checkAInames aiIn
        placeUnits board offset units ais teamIn

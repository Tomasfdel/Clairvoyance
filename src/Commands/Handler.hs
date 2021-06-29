module Commands.Handler where

import AIActions.BreadthFirstSearch
import AIActions.Evaluate
import Commands.Lexer
import Commands.Parser
import Commands.Tokens
import Commands.Types
import Control.Exception
import Control.Monad.Identity
import Control.Monad.Morph
import Control.Monad.State
import qualified Data.List as L
import qualified Data.Vector as V
import Game.BoardGeneration
import Game.GameState
import Game.UnitPlacement
import System.IO

-- ~ Auxiliary function that morphs from State to StateT.
morphStateFunction :: State GameState a -> StateT GameState IO a
morphStateFunction stateFunction = hoist (return . runIdentity) stateFunction

-- ~ Checks the given index corresponds to a unit that is alive.
isValidUnitIndex :: Int -> GameState -> Bool
isValidUnitIndex index gameState = index >= 0 && index < V.length (units gameState) && unitIsAlive ((units gameState) V.! index)

-- ~ Checks the target refers to a valid unit, and returns its index if it does.
evalCommandTarget :: Target -> State GameState (Maybe Int)
evalCommandTarget (Index index) = do
  gameState <- get
  if isValidUnitIndex index gameState
    then return (Just index)
    else return Nothing
evalCommandTarget (Description team name identifier) = do
  gameState <- get
  return
    ( ( V.filter
          (\index -> let unit = (units gameState) V.! index in getTeam unit == team && getName unit == name && getIdentifier unit == identifier && unitIsAlive unit)
          (V.generate (V.length (units gameState)) id)
      )
        V.!? 0
    )

-- ~ Validates if a unit is alive, and kills it if it is.
setUnitDead :: Int -> State GameState Bool
setUnitDead index = do
  gameState <- get
  if isValidUnitIndex index gameState
    then do
      put gameState {units = (units gameState) V.// [(index, updateUnitDead ((units gameState) V.! index))]}
      return True
    else return False

-- ~ Converts a direction to its associated coordinate difference.
directionToOffset :: Direction -> (Int, Int)
directionToOffset DirUp = (0, -1)
directionToOffset DirUpRight = (1, -1)
directionToOffset DirRight = (1, 0)
directionToOffset DirDownRight = (1, 1)
directionToOffset DirDown = (0, 1)
directionToOffset DirDownLeft = (-1, 1)
directionToOffset DirLeft = (-1, 0)
directionToOffset DirUpLeft = (-1, -1)

-- ~ Validates the unit can move according to the movement description and,
-- ~ if it does, moves the unit.
evalCommandMovement :: Int -> Movement -> State GameState (Maybe (Int, Int))
evalCommandMovement index (Position (col, row)) = do
  gameState <- get
  if isValidUnitIndex index gameState
    && validCoord (board gameState) (col, row)
    && ((board gameState) V.! row) V.! col == Empty
    then return (Just (col, row))
    else return Nothing
evalCommandMovement index (Path path) = do
  gameState <- get
  if isValidUnitIndex index gameState
    then
      let position = getPosition ((units gameState) V.! index)
          pathOffsets = map directionToOffset path
          (finalPosition, validSteps) =
            L.mapAccumL
              ( \(col, row) (colDiff, rowDiff) ->
                  let (newCol, newRow) = (col + colDiff, row + rowDiff)
                   in ((newCol, newRow), isValidMovement (board gameState) (col, row) (newCol, newRow))
              )
              position
              pathOffsets
       in if and validSteps
            then return (Just finalPosition)
            else return Nothing
    else return Nothing

-- ~ Validates the given command , modifies the game state according to it and
-- ~ returns whether the command handler should call itself again.
handleCommand :: Command -> StateT GameState IO Bool
handleCommand Next = return False
handleCommand (Move target movement) = do
  evaluatedTarget <- morphStateFunction $ evalCommandTarget target
  case evaluatedTarget of
    Nothing -> do lift $ putStrLn "Invalid command target."
    Just unitIndex -> do
      evaluatedMovement <- morphStateFunction $ evalCommandMovement unitIndex movement
      case evaluatedMovement of
        Nothing -> do lift $ putStrLn "Invalid movement command for that unit."
        Just position -> do morphStateFunction $ moveUnit unitIndex position
  return True
handleCommand (Attack target attack damage) = do
  evaluatedTarget <- morphStateFunction $ evalCommandTarget target
  case evaluatedTarget of
    Nothing -> do lift $ putStrLn "Invalid command target."
    Just unitIndex -> do
      morphStateFunction $ checkAttackHit unitIndex (attack, damage)
      morphStateFunction $ updateIfDead unitIndex
  return True
handleCommand (Kill target) = do
  evaluatedTarget <- morphStateFunction $ evalCommandTarget target
  case evaluatedTarget of
    Nothing -> do lift $ putStrLn "Invalid command target."
    Just unitIndex -> do
      validUnit <- morphStateFunction $ setUnitDead unitIndex
      if validUnit
        then morphStateFunction $ updateIfDead unitIndex
        else return ()
  return True

-- ~ Prompts the user for a command, handles it and is called again until the user says they're done.
commandInput :: StateT GameState IO ()
commandInput = do
  lift $ putStr "> "
  lift $ hFlush stdout
  userInput <- lift getLine
  evaluatedInput <- lift (try (evaluate (parse (alexScanTokens userInput))) :: IO (Either SomeException Command))
  case evaluatedInput of
    Left _ -> do
      lift $ putStrLn "Error interpreting command. Please check your syntax."
      commandInput
    Right command -> do
      shouldContinue <- handleCommand command
      if shouldContinue
        then commandInput
        else return ()

module AIActions.Evaluate where

import AIActions.BreadthFirstSearch
import AIActions.Escape
import AIActions.LineOfSight
import Auxiliary.State
import Control.Monad.State
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as Maybe
import qualified Data.Ord as O
import qualified Data.Set as S
import qualified Data.Vector as V
import FileParser.Types
import Game.BoardGeneration
import Game.Display
import Game.GameState
import Game.StatBlockGeneration
import Game.UnitPlacement
import System.Random

-- ~ Returns a unit predicate for the given unit description.
evalUnitDesc :: GameState -> Int -> UnitDesc -> (Unit -> Bool)
evalUnitDesc gameState index Ally = \u -> unitIsAlive u && (getTeam u) == getTeam ((units gameState) V.! index)
evalUnitDesc gameState index Enemy = \u -> unitIsAlive u && (getTeam u) /= getTeam ((units gameState) V.! index)
evalUnitDesc gameState index (Team name) = \u -> unitIsAlive u && (getTeam u) == name
evalUnitDesc gameState index (TeamUnit team name) = \u -> unitIsAlive u && (getTeam u) == name && (getName u) == name

-- ~ Returns the range of a given attack.
getAttackRange :: AttackDesc -> Int
getAttackRange (Melee, _, _) = 1
getAttackRange (Ranged r, _, _) = r

-- ~ Evaluates a range description and returns the integer value it represents.
evalRange :: GameState -> Int -> Range -> Int
evalRange gameState index (IntR n) = n
evalRange gameState index MeleeR = 1
evalRange gameState index AttackR =
  let Mob current = (units gameState) V.! index
      currAttack = attack (statBlock current)
   in minimum (map getAttackRange currAttack)
evalRange gameState index FullAttackR =
  let Mob current = (units gameState) V.! index
      currFullAttack = fullAttack (statBlock current)
   in minimum (map getAttackRange currFullAttack)
evalRange gameState index SpeedR =
  let Mob current = (units gameState) V.! index
   in speed (statBlock current)
evalRange gameState index (Sum r1 r2) = (evalRange gameState index r1) + (evalRange gameState index r2)
evalRange gameState index (Prod r n) = (evalRange gameState index r) * n

-- ~ Checks the given condition in the current state of the game.
evalCondition :: GameState -> Int -> Condition -> Bool
evalCondition gameState index (UnitCount unitDesc (Comparison comp)) =
  let unitPredicate = evalUnitDesc gameState index unitDesc
      targets = V.filter unitPredicate (units gameState)
   in comp (V.length targets)
evalCondition gameState index (UnitRange unitDesc range) =
  let unitPredicate = evalUnitDesc gameState index unitDesc
      distanceMap = buildFloatingDistanceMap (board gameState) [(getPosition ((units gameState) V.! index))]
      targets = V.filter (\unit -> unitPredicate unit && (isUnitReachable distanceMap unit) && (getUnitDistance distanceMap unit) <= (evalRange gameState index range)) (units gameState)
   in not (V.null targets)
evalCondition gameState index (SpecificUnitRange team name id range) =
  let unitPredicate = (\u -> getTeam u == team && getName u == name && getIdentifier u == id)
      distanceMap = buildFloatingDistanceMap (board gameState) [(getPosition ((units gameState) V.! index))]
      targets = V.filter (\unit -> unitPredicate unit && (isUnitReachable distanceMap unit) && (getUnitDistance distanceMap unit) <= (evalRange gameState index range)) (units gameState)
   in not (null targets)
evalCondition gameState index (UnitRangeCount unitDesc range (Comparison comp)) =
  let unitPredicate = evalUnitDesc gameState index unitDesc
      distanceMap = buildFloatingDistanceMap (board gameState) [(getPosition ((units gameState) V.! index))]
      targets = V.filter (\unit -> unitPredicate unit && (isUnitReachable distanceMap unit) && (getUnitDistance distanceMap unit) <= (evalRange gameState index range)) (units gameState)
   in comp (length targets)
evalCondition gameState index (TotalTurn (Comparison comp)) = comp (turnCount gameState)
evalCondition gameState index (Not cond) = not (evalCondition gameState index cond)
evalCondition gameState index (And c1 c2) = evalCondition gameState index c1 && evalCondition gameState index c2
evalCondition gameState index (Or c1 c2) = evalCondition gameState index c1 || evalCondition gameState index c2

-- ~ Rolls a random number between the given min and max values.
rollDie :: Int -> Int -> State GameState Int
rollDie minVal maxVal = state $ \gameState ->
  let generator = randomGen gameState
      (result, newGen) = randomR (minVal, maxVal) generator
   in (result, gameState {randomGen = newGen})

-- ~ Rolls the dice of a roll description and adds the corresponding modifier.
rollDice :: DieRoll -> State GameState Int
rollDice dieRoll = do
  results <- sequence (replicate (dieAmount dieRoll) (rollDie 1 (dieValue dieRoll)))
  return (modifier dieRoll + sum results)

-- ~ Changes the state board in the given coordinate for the new given tile.
updateStateBoard :: Coordinate -> Tile -> State GameState ()
updateStateBoard (col, row) newTile =
  modify
    (\gameState -> gameState {board = updateBoard (board gameState) (col, row) newTile})

-- ~ Updates the board if a certain unit is dead.
updateIfDead :: Int -> StateT GameState IO ()
updateIfDead index = do
  gameState <- get
  let unit = (units gameState) V.! index
   in if not (unitIsAlive unit)
        then do
          morphStateFunction $ updateStateBoard (getPosition unit) Empty
          lift $ printUnitDeathMessage unit
          return ()
        else return ()

-- ~ Rolls the attack and damage of an attack, given its description.
rollAttack :: AttackDesc -> StateT GameState IO (Int, Int)
rollAttack (_, mod, damage) = do
  attackRoll <- morphStateFunction $ rollDice (DieRoll {dieAmount = 1, dieValue = 20, modifier = mod})
  damageRoll <- morphStateFunction $ rollDice damage
  return (attackRoll, max 1 damageRoll)

-- ~ Checks if an attack (described by its attack and damage rolls) hits a given
-- ~ unit, and updates its health points in case it does.
checkAttackHit :: Maybe Int -> Int -> (Int, Int) -> StateT GameState IO ()
checkAttackHit attackInd defendInd (attackRoll, damageRoll) = do
  gameState <- get
  let attackerUnit = maybe Nothing (\index -> Just ((units gameState) V.! index)) attackInd
      defenderUnit = (units gameState) V.! defendInd
   in do
        lift $ printUnitAttackMessage attackerUnit defenderUnit attackRoll damageRoll
        case defenderUnit of
          (Player _) -> return ()
          (Mob defender) ->
            if attackRoll >= armorClass (statBlock defender)
              then do
                modify
                  ( \gameState ->
                      let newDefender = defender {statBlock = (statBlock defender) {healthPoints = healthPoints (statBlock defender) - damageRoll}}
                          newUnits = (units gameState) V.// [(defendInd, Mob newDefender)]
                       in gameState {units = newUnits}
                  )
                lift $ printAttackHitMessage defenderUnit damageRoll
              else lift $ printAttackMissMessage

-- ~ Rolls all attacks from the attacking unit to the defending unit, and updates
-- ~ the second one in case it's dead after them.
resolveAttack :: Int -> Int -> (MobStatBlock -> [AttackDesc]) -> StateT GameState IO ()
resolveAttack attackInd defendInd attackType = do
  attackerUnit <- gets (\gameState -> (units gameState) V.! attackInd)
  case attackerUnit of
    (Player _) -> return ()
    (Mob attacker) -> do
      modify
        ( \gameState ->
            let newAttacker = attacker {targets = defendInd : (targets attacker)}
             in gameState {units = (units gameState) V.// [(attackInd, Mob newAttacker)]}
        )
      attackRolls <- mapM rollAttack (attackType (statBlock attacker))
      mapM_ (checkAttackHit (Just attackInd) defendInd) attackRolls
      updateIfDead defendInd

-- ~ Moves the given unit to the given coordinate.
moveUnit :: Int -> Coordinate -> StateT GameState IO ()
moveUnit index (newCol, newRow) = do
  unit <- gets (\gameState -> (units gameState) V.! index)
  morphStateFunction $ updateStateBoard (getPosition unit) Empty
  modify
    ( \gameState ->
        let newUnit = updateUnitPosition unit (newCol, newRow)
         in gameState {units = (units gameState) V.// [(index, newUnit)]}
    )
  morphStateFunction $ updateStateBoard (newCol, newRow) (Unit index)
  lift $ printUnitMovementMessage unit (newCol, newRow)

-- ~ Returns a list of all unit indices in the given set that are present
-- ~ in the given target history.
findUnitsInHistory :: (S.Set Int) -> [Int] -> [Int]
findUnitsInHistory _ [] = []
findUnitsInHistory unitSet (index : is) =
  if S.member index unitSet
    then index : (findUnitsInHistory (S.delete index unitSet) is)
    else findUnitsInHistory unitSet is

-- ~ Sorts a list of unit indexes from most to least appropriate
-- ~ according to the given adjective.
sortByAdjective :: GameState -> Board DistanceMapTile -> Int -> [Int] -> Adjective -> [Int]
sortByAdjective gameState distanceMap _ searchUnits Closest =
  L.sortBy
    (O.comparing (\index -> getUnitDistance distanceMap ((units gameState) V.! index)))
    (filter (\index -> isUnitReachable distanceMap ((units gameState) V.! index)) searchUnits)
sortByAdjective gameState distanceMap index searchUnits Furthest = reverse (sortByAdjective gameState distanceMap index searchUnits Closest)
sortByAdjective gameState _ index searchUnits Last =
  let targetHistory = getTargets ((units gameState) V.! index)
   in findUnitsInHistory (S.fromList searchUnits) targetHistory

-- ~ Returns the index of the unit that best fits a target definition.
evaluateTarget :: GameState -> Board DistanceMapTile -> Int -> Target -> Maybe Int
evaluateTarget _ _ index Self = Just index
evaluateTarget gameState _ index (Specific team name identifier) =
  let unitIndices = V.generate (V.length (units gameState)) id
      targetUnit = V.filter (\index -> let unit = (units gameState) V.! index in unitIsAlive unit && getTeam unit == team && getName unit == name && getIdentifier unit == identifier) unitIndices
   in targetUnit V.!? 0
evaluateTarget gameState distanceMap index (Description adjective unitDesc) =
  let unitIndices = V.generate (V.length (units gameState)) id
      unitPred = evalUnitDesc gameState index unitDesc
      possibleTargets = V.filter (\index -> unitPred ((units gameState) V.! index)) unitIndices
      sortedTargets = sortByAdjective gameState distanceMap index (V.toList possibleTargets) adjective
   in Maybe.listToMaybe sortedTargets

-- ~ Moves a unit as far as possible in the direction of the given coordinate.
-- ~ The boolean argument indicates whether the unit should look to step into
-- ~ the given tile or get in one of its adjacent tiles.
moveTowardsPosition :: Int -> Board DistanceMapTile -> Coordinate -> Int -> Bool -> StateT GameState IO ()
moveTowardsPosition index distanceMap position maxDistance trimLast =
  let pathToTarget = buildPathToTarget position distanceMap
      realPath = if trimLast then tail (pathToTarget) else pathToTarget
   in case Maybe.listToMaybe (filter (\(_, distance) -> distance <= maxDistance) realPath) of
        Nothing -> return ()
        Just ((newCol, newRow), _) -> moveUnit index (newCol, newRow)

-- ~ Evaluates a unit's attack action.
evalAttackAction :: Int -> Target -> Bool -> StateT GameState IO Bool
evalAttackAction index target isFullAttack = do
  gameState <- get
  let attackerPosition = getPosition ((units gameState) V.! index)
      distanceMap = buildWalkingDistanceMap (board gameState) [attackerPosition]
   in case evaluateTarget gameState distanceMap index target of
        Nothing -> return True
        Just targetIndex ->
          let attackFunction = if isFullAttack then fullAttack else attack
              targetPosition = getPosition ((units gameState) V.! targetIndex)
              attackRange = minimum (map getAttackRange (attackFunction (getMobStatBlock ((units gameState) V.! index))))
              validAttackPositions = getValidAttackPositions (board gameState) targetPosition attackRange
           in if S.member attackerPosition validAttackPositions
                then do
                  resolveAttack index targetIndex attackFunction
                  return True
                else
                  let possiblePosition = head (L.sortOn (\(col, row) -> tileDistance ((distanceMap V.! row) V.! col)) (S.toList validAttackPositions))
                      unitSpeed = speed (getMobStatBlock ((units gameState) V.! index))
                      maxDistance = if isFullAttack then 2 * unitSpeed else unitSpeed
                   in do
                        moveTowardsPosition index distanceMap possiblePosition maxDistance False
                        return False

-- ~ Evaluates a unit's action and returns whether it was fully completed.
-- ~ In the case of having an invalid action target, the function returns the action could be
-- ~ carried out since it will never be able to be fully completed.
evalAction :: Int -> TurnAction -> StateT GameState IO Bool
evalAction index (Move (Approach target)) = do
  gameState <- get
  let distanceMap = buildWalkingDistanceMap (board gameState) [(getPosition ((units gameState) V.! index))]
   in case evaluateTarget gameState distanceMap index target of
        Nothing -> return True
        Just targetIndex ->
          let targetPosition = (getPosition ((units gameState) V.! targetIndex))
              unitSpeed = speed (getMobStatBlock ((units gameState) V.! index))
           in do
                moveTowardsPosition index distanceMap targetPosition unitSpeed True
                return True
evalAction index (Move Disengage) = do
  gameState <- get
  let currentPosition = getPosition ((units gameState) V.! index)
      unitSpeed = speed (getMobStatBlock ((units gameState) V.! index))
      enemyPredicate = evalUnitDesc gameState index Enemy
      enemyPositions = V.map getPosition (V.filter enemyPredicate (units gameState))
      distanceMap = buildWalkingDistanceMap (board gameState) (V.toList enemyPositions)
      newPosition = findEscapePosition (board gameState) (buildEscapeMap (board gameState) distanceMap) currentPosition (fromIntegral unitSpeed)
   in do
        moveUnit index newPosition
        return True
evalAction index (Standard (AttackAction target)) = evalAttackAction index target False
evalAction index (Full (FullAttackAction target)) = evalAttackAction index target True

-- ~ Evaluates all actions in a unit's turn and returns whether all of them could be carried out.
evalTurn :: Int -> [TurnAction] -> StateT GameState IO Bool
evalTurn _ [] = return True
evalTurn _ (Pass : as) = return True
evalTurn index (action : as) = do
  result <- evalAction index action
  restResult <- evalTurn index as
  return (result && restResult)

-- ~ Evaluates one turn of the given unit's AI action and returns its
-- ~ advanced action and whether anything was carried out.
aiStep :: Int -> Action -> StateT GameState IO (Action, Bool)
aiStep index None = return (None, False)
aiStep index (Turn ts) = do
  shouldAdvance <- evalTurn index ts
  let newAction = if shouldAdvance then None else (Turn ts)
   in return (newAction, True)
aiStep index (If cond tAct fAct) = do
  gameState <- get
  let nextAction = if evalCondition gameState index cond then tAct else fAct
   in aiStep index nextAction
aiStep index (While cond action) = aiStep index (If cond (Cons action (While cond action)) None)
aiStep index (Cons first second) = do
  (newFirst, playedTurn) <- aiStep index first
  if playedTurn
    then return (Cons newFirst second, True)
    else aiStep index second

-- ~ Updates the AI actions of the unit with the given index.
updateUnitAI :: Int -> Action -> State GameState ()
updateUnitAI index newAI = do
  gameState <- get
  let Mob newUnit = (units gameState) V.! index
      newUnits = (units gameState) V.// [(index, Mob (newUnit {ai = newAI}))]
   in put (gameState {units = newUnits})

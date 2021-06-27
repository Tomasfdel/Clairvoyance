module AIActions where

import BoardGeneration
import BreadthFirstSearch
import Control.Monad.State
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as Maybe
import qualified Data.Ord as O
import qualified Data.Set as S
import qualified Data.Vector as V
import Escape
import GameState
import LineOfSight
import ParserTypes
import StatBlockGeneration
import System.Random
import UnitPlacement

-- ~ Checks that there are no duplicate names in the AI actions list.
buildAImap :: [AIInput] -> (M.Map String Action) -> Either String (M.Map String Action)
buildAImap [] aiMap = Right aiMap
buildAImap ((name, ai) : ais) aiMap =
  if M.member name aiMap
    then Left ("Dupĺicate AI name " ++ name ++ ".")
    else buildAImap ais (M.insert name ai aiMap)

-- ~ Verifies the names of the listed AI actions.
checkAInames :: [AIInput] -> Either String (M.Map String Action)
checkAInames ais = buildAImap ais M.empty

-- ~ Returns a unit predicate for the given unit description.
evalUnitDesc :: GameState -> Int -> UnitDesc -> (Unit -> Bool)
evalUnitDesc gameState index Ally = \u -> unitIsAlive u && (getTeam u) == getTeam ((units gameState) V.! index)
evalUnitDesc gameState index Enemy = \u -> unitIsAlive u && (getTeam u) /= getTeam ((units gameState) V.! index)
evalUnitDesc gameState index (Team name) = \u -> unitIsAlive u && (getTeam u) == name
evalUnitDesc gameState index (TeamUnit team name) = \u -> unitIsAlive u && (getTeam u) == name && (getName u) == name

getAttackRange :: AttackDesc -> Int
getAttackRange (Melee, _, _) = 1
getAttackRange (Ranged r, _, _) = r

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

-- ~ Checks the given condition the current state of the game.
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

rollDie :: Int -> Int -> State GameState Int
rollDie minVal maxVal = state $ \gameState ->
  let generator = randomGen gameState
      (result, newGen) = randomR (minVal, maxVal) generator
   in (result, gameState {randomGen = newGen})

-- TO DO: Revisar si me conviene que sea un dieRoll o que sea una tripla
rollDice :: DieRoll -> State GameState Int
rollDice dieRoll = do
  results <- sequence (replicate (dieAmount dieRoll) (rollDie 1 (dieValue dieRoll)))
  return (modifier dieRoll + sum results)

updateBoard :: Coordinate -> Tile -> State GameState ()
updateBoard (col, row) newTile =
  modify
    ( \gameState ->
        let newRow = ((board gameState) V.! row) V.// [(col, newTile)]
            newBoard = (board gameState) V.// [(row, newRow)]
         in gameState {board = newBoard}
    )

updateIfDead :: Int -> State GameState ()
updateIfDead index = do
  gameState <- get
  let unit = (units gameState) V.! index
   in if not (unitIsAlive unit)
        then do
          updateBoard (getPosition unit) Empty
          return ()
        else return ()

rollAttack :: AttackDesc -> State GameState (Int, Int)
rollAttack (_, mod, damage) = do
  attackRoll <- rollDice (DieRoll {dieAmount = 1, dieValue = 20, modifier = mod})
  damageRoll <- rollDice damage
  return (attackRoll, max 1 damageRoll)

checkAttackHit :: Int -> (Int, Int) -> State GameState ()
checkAttackHit defendInd (attackRoll, damageRoll) = do
  (Mob defender) <- gets (\gameState -> (units gameState) V.! defendInd)
  if attackRoll >= armorClass (statBlock defender)
    then
      modify
        ( \gameState ->
            let newDefender = defender {statBlock = (statBlock defender) {healthPoints = healthPoints (statBlock defender) - damageRoll}}
                newUnits = (units gameState) V.// [(defendInd, Mob newDefender)]
             in gameState {units = newUnits}
        )
    else return ()

resolveAttack :: Int -> Int -> (StatBlock -> [AttackDesc]) -> State GameState ()
resolveAttack attackInd defendInd attackType = do
  (Mob attacker) <- gets (\gameState -> (units gameState) V.! attackInd)
  modify
    ( \gameState ->
        let newAttacker = attacker {targets = defendInd : (targets attacker)}
         in gameState {units = (units gameState) V.// [(attackInd, Mob newAttacker)]}
    )
  attackRolls <- mapM rollAttack (attackType (statBlock attacker))
  mapM_ (checkAttackHit defendInd) attackRolls
  updateIfDead defendInd

moveUnit :: Int -> Coordinate -> State GameState ()
moveUnit index (newCol, newRow) = do
  (Mob unit) <- gets (\gameState -> (units gameState) V.! index)
  updateBoard (position unit) Empty
  modify
    ( \gameState ->
        let newUnit = unit {position = (newCol, newRow)}
         in gameState {units = (units gameState) V.// [(index, Mob newUnit)]}
    )
  updateBoard (newCol, newRow) (Unit index)

unitToInt :: GameState -> Unit -> Int
unitToInt state (Mob unit) =
  let (col, row) = position unit
      Unit index = ((board state) V.! row) V.! col
   in index

intToUnit :: GameState -> Int -> Unit
intToUnit state index = (units state) V.! index

getUnitDistance :: Board DistanceMapTile -> Unit -> Int
getUnitDistance distanceMap unit =
  let (col, row) = getPosition unit
   in floor (snd ((distanceMap V.! row) V.! col))

isUnitReachable :: Board DistanceMapTile -> Unit -> Bool
isUnitReachable distanceMap unit = (getUnitDistance distanceMap unit) >= 0

findUnitsInHistory :: (S.Set Int) -> [Int] -> [Int]
findUnitsInHistory _ [] = []
findUnitsInHistory unitSet (index : is) =
  if S.member index unitSet
    then index : (findUnitsInHistory (S.delete index unitSet) is)
    else findUnitsInHistory unitSet is

sortByAdjective :: GameState -> Board DistanceMapTile -> Int -> [Int] -> Adjective -> [Int]
sortByAdjective gameState distanceMap _ searchUnits Closest =
  L.sortBy
    (O.comparing (\index -> getUnitDistance distanceMap ((units gameState) V.! index)))
    (filter (\index -> isUnitReachable distanceMap ((units gameState) V.! index)) searchUnits)
sortByAdjective gameState distanceMap index searchUnits Furthest = reverse (sortByAdjective gameState distanceMap index searchUnits Closest)
sortByAdjective gameState _ _ searchUnits LeastInjured = L.sortBy (O.comparing (\index -> healthPoints (getStatBlock ((units gameState) V.! index)))) searchUnits
sortByAdjective gameState distanceMap index searchUnits MostInjured = reverse (sortByAdjective gameState distanceMap index searchUnits LeastInjured)
sortByAdjective gameState _ index searchUnits Last =
  let targetHistory = getTargets ((units gameState) V.! index)
   in findUnitsInHistory (S.fromList searchUnits) targetHistory

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

moveTowardsPosition :: Int -> Board DistanceMapTile -> Coordinate -> Int -> Bool -> State GameState ()
moveTowardsPosition index distanceMap position maxDistance trimLast =
  let pathToTarget = buildPathToTarget position distanceMap
      realPath = if trimLast then tail (pathToTarget) else pathToTarget
   in case Maybe.listToMaybe (filter (\(_, distance) -> distance <= maxDistance) realPath) of
        Nothing -> return ()
        Just ((newCol, newRow), _) -> moveUnit index (newCol, newRow)

evalAttackAction :: Int -> Target -> Bool -> State GameState Bool
evalAttackAction index target isFullAttack = do
  gameState <- get
  let attackerPosition = getPosition ((units gameState) V.! index)
      distanceMap = buildWalkingDistanceMap (board gameState) [attackerPosition]
   in case evaluateTarget gameState distanceMap index target of
        Nothing -> return True
        Just targetIndex ->
          let attackFunction = if isFullAttack then fullAttack else attack
              targetPosition = getPosition ((units gameState) V.! targetIndex)
              attackRange = minimum (map getAttackRange (attackFunction (getStatBlock ((units gameState) V.! index))))
              validAttackPositions = getValidAttackPositions (board gameState) targetPosition attackRange
           in if S.member attackerPosition validAttackPositions
                then do
                  resolveAttack index targetIndex attackFunction
                  return True
                else
                  let possiblePosition = head (L.sortOn (\(col, row) -> tileDistance ((distanceMap V.! row) V.! col)) (S.toList validAttackPositions))
                      unitSpeed = speed (getStatBlock ((units gameState) V.! index))
                      maxDistance = if isFullAttack then 2 * unitSpeed else unitSpeed
                   in do
                        moveTowardsPosition index distanceMap possiblePosition maxDistance False
                        return False

evalAction :: Int -> TurnAction -> State GameState Bool
evalAction index (Move (Approach target)) = do
  gameState <- get
  let distanceMap = buildWalkingDistanceMap (board gameState) [(getPosition ((units gameState) V.! index))]
   in case evaluateTarget gameState distanceMap index target of
        Nothing -> return True
        Just targetIndex ->
          let targetPosition = (getPosition ((units gameState) V.! targetIndex))
              unitSpeed = speed (getStatBlock ((units gameState) V.! index))
           in do
                moveTowardsPosition index distanceMap targetPosition unitSpeed True
                return True
evalAction index (Move Disengage) = do
  gameState <- get
  let currentPosition = getPosition ((units gameState) V.! index)
      unitSpeed = speed (getStatBlock ((units gameState) V.! index))
      enemyPredicate = evalUnitDesc gameState index Enemy
      enemyPositions = V.map getPosition (V.filter enemyPredicate (units gameState))
      distanceMap = buildWalkingDistanceMap (board gameState) (V.toList enemyPositions)
      newPosition = findEscapePosition (board gameState) (buildEscapeMap (board gameState) distanceMap) currentPosition (fromIntegral unitSpeed)
   in do
        moveUnit index newPosition
        return True
evalAction index (Standard (AttackAction target)) = evalAttackAction index target False
evalAction index (Full (FullAttackAction target)) = evalAttackAction index target True

evalTurn :: Int -> [TurnAction] -> State GameState Bool
evalTurn _ [] = return True
evalTurn _ (Pass : as) = return True
evalTurn index (action : as) = do
  result <- evalAction index action
  restResult <- evalTurn index as
  return (result && restResult)

-- ~ Evaluates one turn of the given unit's AI action and returns its
-- ~ advanced action and whether anything was carried out.
aiStep :: Int -> Action -> State GameState (Action, Bool)
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

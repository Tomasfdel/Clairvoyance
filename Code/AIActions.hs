module AIActions where

import ParserTypes 
import qualified Data.Map as M

buildAImap :: [AIInput] -> (M.Map String Action) -> Either String (M.Map String Action)
buildAImap [] aiMap = Right aiMap
buildAImap ((name, ai) : ais) aiMap = if M.member name aiMap then Left ("Dupĺicate AI name " ++ name ++ ".")
                                                             else buildAImap ais (M.insert name ai aiMap)

checkAInames :: [AIInput] -> Either String (M.Map String Action)
checkAInames ais = buildAImap ais M.empty





evalCondition :: Board -> V.Vector Unit -> Int -> Condition -> Bool
evalCondition board units index (Not cond) = not (evalCondition board units index cond)
evalCondition board units index (And c1 c2) = evalCondition board units index c1 && evalCondition board units index c2
evalCondition board units index (Or c1 c2) = evalCondition board units index c1 || evalCondition board units index c2


-- ~ evalMoveAction :: Board -> V.Vector Unit -> Int -> MoveAction -> (Board, V.Vector Unit, Bool)
-- ~ evalMoveAction board units index (Approach target) = 
-- ~ evalMoveAction board units index Defend =


-- ~ evalStandardAction :: Board -> V.Vector Unit -> Int -> StadardAction -> (Board, V.Vector Unit, Bool)
-- ~ evalStandardAction board units index (Attack target) = 


-- ~ evalFullAction :: Board -> V.Vector Unit -> Int -> FullAction -> (Board, V.Vector Unit, Bool)
-- ~ evalFullAction board units index (FullAttack target) = 


-- TO DO: Dejar de hacer esto tail recursive cuando vea cómo meter State en todo esto.
evalTurn :: Board -> V.Vector Unit -> Int -> [TurnAction] -> Bool -> IO(Board, V.Vector Unit, Bool)
evalTurn board units _ [] result = return (board, units, result)
evalTurn board units _ (Pass : as) result = return (board, units, result && true)
evalTurn board units index ((Move ma) : as) result = do (newBoard, newUnits, maResult) <- evalMoveAction board units index ma 
                                                        evalTurn newBoard newUnits index as (result && maResult)
evalTurn board units index ((Standard sa) : as) result = do (newBoard, newUnits, maResult) <- evalStandardAction board units index sa 
                                                            evalTurn newBoard newUnits index as (result && saResult)
evalTurn board units index ((Full fa) : as) result = do (newBoard, newUnits, faResult) <- evalFullAction board units index fa 
                                                        evalTurn newBoard newUnits index as (result && faResult)


aiStep -> Board -> V.Vector Unit -> Int -> Action -> IO(Board, V.Vector Unit, Action, Bool)
aiStep board units index None = return (board, units, None, False)
aiStep board units index (Turn ts) = do (newBoard, newUnits, success) <- evalTurn board units index ts true
                                        if success then return (newBoard, newUnits, None, success)
                                                   else return (newBoard, newUnits, Turn ts, success)
aiStep board units index (If cond tAct fAct) = let nextAction = if evalCondition board units index cond then tAct else fAct
                                                in aiStep board units index nextAction
aiStep board units index (Cons first second) = do firstResult <- aiStep board units index first
                                                  case firstResult of
                                                       (newBoard, newUnits, None, playedTurn) -> if playedTurn then return (newBoard, newUnits, second, playedTurn)
                                                                                                               else aiStep board units index second
                                                       (newBoard, newUnits, newFirst, _) -> return (newBoard, newUnits, Cons newFirst second)
aiStep board units index (While cond action) = if evalCondition board units index cond then aiStep board units index (Cons action (While cond action))
                                                                                       else return (board, units, None, False)

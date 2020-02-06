module Game where

import BoardGeneration
import StatBlockGeneration
import UnitPlacement
import GameState
import AIActions
import qualified Data.List as L
import qualified Data.Vector as V
import qualified System.Random as R
                             
takeTurn :: GameState -> Int -> IO(GameState)
takeTurn gameState index = case (units gameState) V.! index of
                                Mob unit -> do (newState, newAI, _) <- aiStep gameState index (getAI ((units gameState) V.! index))
                                               return (newState { units = (units newState) V.// [(index, Mob (unit { ai = newAI }))] })


getMobInitiative :: (V.Vector Unit) -> Int -> Int
getMobInitiative units ind = let Mob unit = units V.! ind
                              in initiative (statBlock unit)

initiativeDieRoll :: (Int, Int) -> IO (Int, Int)
initiativeDieRoll (index, mod) = do result <- R.randomRIO (1,20)
                                    return (index, result + mod)

initiativeDiceRolls :: (V.Vector Unit) -> [Int] -> IO([(Int, Int)])
initiativeDiceRolls units indices = let indexWithMod = map (\i -> (i, getInitiative (units V.! i))) indices
                                     in mapM initiativeDieRoll indexWithMod

regroupInitiatives :: [(Int, Int)] -> [[Int]]
regroupInitiatives rolls = let reorder = (reverse (L.sortOn snd rolls))
                               tupleGroups = L.groupBy (\x y -> (snd x) == (snd y)) reorder
                            in map (map fst) tupleGroups

initiativeReorder :: (V.Vector Unit) -> [Int] -> IO([Int])
initiativeReorder _ [] = return []
initiativeReorder _ [n] = return [n]
initiativeReorder units indices = do results <- initiativeDiceRolls units indices 
                                     reorderedGroups <- mapM (initiativeReorder units) (regroupInitiatives results)
                                     return (concat reorderedGroups)

initiativeRoll :: (V.Vector Unit) -> IO(V.Vector Int)
initiativeRoll units = do initList <- initiativeReorder units [0 .. (V.length units - 1)]
                          return (V.fromList initList)

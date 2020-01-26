module AIActions where

import ParserTypes 
import qualified Data.Map as M

buildAImap :: [AIInput] -> (M.Map String Action) -> Either String (M.Map String Action)
buildAImap [] aiMap = Right aiMap
buildAImap ((name, ai) : ais) aiMap = if M.member name aiMap then Left ("Dupĺicate AI name " ++ name ++ ".")
                                                             else buildAImap ais (M.insert name ai aiMap)

checkAInames :: [AIInput] -> Either String (M.Map String Action)
checkAInames ais = buildAImap ais M.empty

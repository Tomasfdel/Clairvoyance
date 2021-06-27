{
module Lexer (alexScanTokens) where

import LexerTokens
import Data.Char

}

%wrapper "basic"

$digit = 0-9      -- digits
$alpha = [a-zA-Z]   -- alphabetic characters

tokens :-
	$white+        ;
	"//".*         ;
	"MAP"          { \s -> LTMap }
	"Layout"       { \s -> LTLayout }
	"Rectangle"    { \s -> LTRectangle }
	"Outline"      { \s -> LTOutline }
	"Obstacles"    { \s -> LTObstacles }
	"U"            { \s -> LTDirUp }
	"R"            { \s -> LTDirRight }
	"D"            { \s -> LTDirDown }
	"L"            { \s -> LTDirLeft }	
	"UNIT"         { \s -> LTUnit }
	"HP"           { \s -> LTHP }
	"Initiative"   { \s -> LTInitiative }
	"Speed"        { \s -> LTSpeed }
	"AC"           { \s -> LTAC }
	"Attack"       { \s -> LTAttack }
	"Full Attack"  { \s -> LTFullAttack }
	"Melee"        { \s -> LTMelee }
	"Ranged"       { \s -> LTRanged }
	"TEAM"         { \s -> LTTeam }	
	"AI"           { \s -> LTAI }
	"Pass"         { \s -> LTPass }
	"if"           { \s -> LTIf }
	"then"         { \s -> LTThen } 
	"else"         { \s -> LTElse }
	"while"        { \s -> LTWhile }
	"not"          { \s -> LTNot }
	"and"          { \s -> LTAnd }
	"or"           { \s -> LTOr }
	"Approach"     { \s -> LTApproach }
	"Disengage"    { \s -> LTDisengage }
	"ally"         { \s -> LTAlly }
	"enemy"        { \s -> LTEnemy }
	"self"         { \s -> LTSelf }
	"closest"      { \s -> LTClosest }
	"furthest"     { \s -> LTFurthest }
	"most"         { \s -> LTMost }
	"least"        { \s -> LTLeast }
	"injured"      { \s -> LTInjured }
	"last"         { \s -> LTLast }
	"count"        { \s -> LTCount }
	"in"           { \s -> LTIn }
	"range"        { \s -> LTRange }
	"total"        { \s -> LTTotal }
	"turn"         { \s -> LTTurn }
	$digit+        { \s -> LTNat (read s) }
	$digit+ "d" $digit+             { \s -> LTDie (read (takeWhile isDigit s), read (tail (dropWhile isDigit s))) }
	"x" $digit+                     { \s -> LTMult (read (tail s)) }
	[d\+\-\,\;\:\{\}\(\)\*\<\>\=\!]         { \s -> LTSym (head s) }
	$alpha [$alpha $digit \_ \’]*   { \s -> LTVar s }

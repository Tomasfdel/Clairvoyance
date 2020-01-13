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
	"Spells"       { \s -> LTSpells }
	"Saves"        { \s -> LTSaves }
	"Fortitude"    { \s -> LTFortitude }
	"Reflex"       { \s -> LTReflex }
	"Will"         { \s -> LTWill }
	"TEAM"         { \s -> LTTeam }	
	$digit+        { \s -> LTInt (read s) }
	$digit+ "d" $digit+             { \s -> LTDie (read (takeWhile isDigit s), read (tail (dropWhile isDigit s))) }
	"x" $digit+                     { \s -> LTCount (read (tail s)) }
	[d\+\-\,\;\:\{\}\(\)]           { \s -> LTSym (head s) }
	$alpha [$alpha $digit \_ \’]*   { \s -> LTVar s }

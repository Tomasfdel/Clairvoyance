{
module Commands.Lexer (alexScanTokens) where

import Commands.Tokens

}

%wrapper "basic"

$digit = 0-9      -- digits
$alpha = [a-zA-Z]   -- alphabetic characters

tokens :-
	$white+        ;
    "next"         { \s -> LTNext }
    "show"         { \s -> LTShow }
    "board"        { \s -> LTBoard }
    "move"         { \s -> LTMove }
    "U"            { \s -> LTUp }
    "UR"           { \s -> LTUpRight }
    "RU"           { \s -> LTUpRight }
    "R"            { \s -> LTRight }
    "DR"            { \s -> LTDownRight }
    "RD"            { \s -> LTDownRight }
    "D"            { \s -> LTDown }
    "DL"            { \s -> LTDownLeft }
    "LD"            { \s -> LTDownLeft }
    "L"            { \s -> LTLeft }
    "UL"            { \s -> LTUpLeft }
    "LU"            { \s -> LTUpLeft }
    "attack"       { \s -> LTAttack }
    "kill"         { \s -> LTKill }
	$digit+        { \s -> LTNat (read s) }
	[\:\-\(\)\,]           { \s -> LTSym (head s) }
	$alpha [$alpha $digit \_ \’]*   { \s -> LTVar s }

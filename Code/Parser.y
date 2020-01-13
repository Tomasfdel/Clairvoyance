{
module Parser where

import LexerTokens

}

%name parse
%tokentype { LexerToken }
%error { parseError }

%token
	Map            { LTMap }
	Layout         { LTLayout }
    Rectangle      { LTRectangle }
    Outline        { LTOutline }
	Obstacles      { LTObstacles }  
	U              { LTDirUp }
	R              { LTDirRight }
	D              { LTDirDown }
	L              { LTDirLeft }
	Int            { LTInt $$ }
	Die            { LTDie $$ }
	Count          { LTCount $$ }
	Name           { LTVar $$ }
	Unit           { LTUnit }
	HP             { LTHP }
	Initiative     { LTInitiative }
	Speed          { LTSpeed }
	AC             { LTAC }
	Attack         { LTAttack }
	FullAttack     { LTFullAttack }
	Melee          { LTMelee }
	Ranged         { LTRanged }
	Saves          { LTSaves }
	Fortitude      { LTFortitude }
	Reflex         { LTReflex }
	Will           { LTWill }
	Team           { LTTeam }
	'd'            { LTSym 'd' }
	'+'            { LTSym '+' }
	'-'            { LTSym '-' }
	','            { LTSym ',' }
	';'            { LTSym ';' }
	':'            { LTSym ':' }
	'{'            { LTSym '{' }
	'}'            { LTSym '}' }
	'('            { LTSym '(' }
    ')'            { LTSym ')' }
    

%%


Game : Board UnitList TeamList                            { ($1, $2, $3) } 

Board : Map '{' Layout ':' LayoutDesc ';' Obstacles ':' ObstacleList ';' '}'    { ($5, $9) }

LayoutDesc : Rectangle Int ',' Int                        { Rectangle $2 $4 }
           | Outline DirectionList                        { Outline $2 }

DirectionList : Int Direction                             { [($1, $2)] }
              | Int Direction '-' DirectionList           { ($1, $2) : $4 }

Direction : U                                             { DirUp }
          | R                                             { DirRight }
          | D                                             { DirDown }
          | L                                             { DirLeft }

ObstacleList : {- empty -}                                     { [] }          
             | '(' IntRange ',' IntRange ')' ObstacleTail      { ($2, $4) : $6 }

ObstacleTail : {- empty -}                                     { [] }
             | ',' '(' IntRange ',' IntRange ')' ObstacleTail  { ($3, $5) : $7 }  

IntRange : Int                                            { ($1, $1) }
         | Int '-' Int                                    { ($1, $3) }

UnitList : Unit Name '{' StatList '}'                     { [($2, $4)] }
         | Unit Name '{' StatList '}' UnitList            { ($2, $4) : $6 }

StatList : UnitStat ';'                                   { [$1] }
         | UnitStat ';' StatList                          { $1 : $3 }

UnitStat : HP ':' Int                                     { HP $3 }
         | Initiative ':' Modifier                        { Initiative $3 }
         | Speed ':' Int                                  { Speed $3 }
         | AC ':' Int                                     { AC $3 }
	     | Attack ':' AttackDesc                          { Attack $3 }
	     | FullAttack ':' AttackDescList                  { FullAttack $3 }
	     | Saves ':' SavesList                            { Saves $3 }
	     
AttackDescList : AttackDesc                               { [$1] }
	           | AttackDesc ',' AttackDescList            { $1 : $3 }

AttackDesc : AttackRange Modifier DieRoll                 { ($1, $2, $3) }
           | AttackRange DieRoll                          { ($1,  0, $2) }

AttackRange : Melee                                       { Melee }
            | Ranged '(' Int ')'                          { Ranged $3 }
            
Modifier : '+' Int                                        { $2 }
         | '-' Int                                        { - $2 }

DieRoll : DiceExp                                         { DieRoll {dieAmount = fst $1, dieValue = snd $1, modifier = 0} }
        | DiceExp Modifier                                { DieRoll {dieAmount = fst $1, dieValue = snd $1, modifier = $2} }

DiceExp : Int 'd' Int                                     { ($1, $3) }
        | Die                                             { $1 }
            
SavesList : UnitSave                                      { [$1] }
          | UnitSave ',' SavesList                        { $1 : $3 }

UnitSave : Fortitude ':' Modifier                         { Fortitude $3 }
         | Reflex ':' Modifier                            { Reflex $3 }
         | Will ':' Modifier                              { Will $3 }

TeamList : Team Name '{' TeamMemberList '}'               { [($2, $4)] }
         | Team Name '{' TeamMemberList '}' TeamList      { ($2, $4) : $6 }

TeamMemberList : TeamMember ';'                           { [$1] }
               | TeamMember ';' TeamMemberList            { $1 : $3 } 

TeamMember : Name Count ':' CoordinateList                { ($1, $2, $4) }

CoordinateList : '(' Int ',' Int ')'                      { [($2, $4)] }
               | '(' Int ',' Int ')' ',' CoordinateList   { ($2, $4) : $7 }  

{

data Direction = DirUp | DirRight | DirDown | DirLeft 
                 deriving Show

data Layout = Rectangle Int Int | Outline [(Int, Direction)] 
              deriving Show

type Coordinate = (Int, Int)

type Obstacle = (Coordinate, Coordinate) 

type Map = (Layout, [Obstacle])

data DieRoll = DieRoll { dieAmount :: Int,
                         dieValue :: Int, 
                         modifier :: Int } 
               deriving Show

data AttackRange = Melee | Ranged Int 
                   deriving Show

type AttackDesc = (AttackRange, Int, DieRoll)

data UnitSave = 
     Fortitude Int |
     Reflex Int    |
     Will Int 
     deriving Show

data UnitStat = 
	HP Int                   |
	Initiative Int           |
	Speed Int                |
	AC Int                   |
	Attack AttackDesc        |
	FullAttack [AttackDesc]  |
	Saves [UnitSave] 
	deriving Show

type StatBlock = (String, [UnitStat])

type Team = (String, [(String, Int, [Coordinate])])

type Game = (Map, [StatBlock], [Team])


parseError :: [LexerToken] -> a
parseError _ = error "Parse error"

}


 


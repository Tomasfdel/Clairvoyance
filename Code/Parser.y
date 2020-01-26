{
module Parser where

import LexerTokens
import ParserTypes

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
	Nat            { LTNat $$ }
	Die            { LTDie $$ }
	Mult           { LTMult $$ }
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
    '*'            { LTSym '*' }
    '<'            { LTSym '<' }
    '>'            { LTSym '>' }
    '='            { LTSym '=' }
    '!'            { LTSym '!' }
    AI             { LTAI }
    Pass           { LTPass }
    If             { LTIf }
    Then           { LTThen }
    Else           { LTElse }
    While          { LTWhile }
    Not            { LTNot }
    And            { LTAnd }
    Or             { LTOr }
    Approach       { LTApproach }
    Defend         { LTDefend }
    Ally           { LTAlly }
    Enemy          { LTEnemy }
    Self           { LTSelf }
    Closest        { LTClosest }
    Furthest       { LTFurthest }
    Most           { LTMost }
    Least          { LTLeast }
    Injured        { LTInjured }
    Last           { LTLast }
    Count          { LTCount }
    In             { LTIn }
    Range          { LTRange }
    Total          { LTTotal }
    Turn           { LTTurn }

%right Or
%right And
%right Not
%left '+'
%left '*'

%%


Game : Board UnitList AIList TeamList                     { ($1, $2, $3, $4) } 

Board : Map '{' Layout ':' LayoutDesc ';' Obstacles ':' ObstacleList ';' '}'    { ($5, $9) }

LayoutDesc : Rectangle Nat ',' Nat                        { Rectangle $2 $4 }
           | Outline DirectionList                        { Outline $2 }

DirectionList : Nat Direction                             { [($1, $2)] }
              | Nat Direction '-' DirectionList           { ($1, $2) : $4 }

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
         
Int : Nat                                                 { $1 }
    | '-' Nat                                             { -$2 }

UnitList : Unit Name '{' StatList '}'                     { [($2, $4)] }
         | Unit Name '{' StatList '}' UnitList            { ($2, $4) : $6 }

StatList : UnitStat ';'                                   { [$1] }
         | UnitStat ';' StatList                          { $1 : $3 }

UnitStat : HP ':' Nat                                     { HP $3 }
         | Initiative ':' Modifier                        { Initiative $3 }
         | Speed ':' Nat                                  { Speed $3 }
         | AC ':' Int                                     { AC $3 }
	     | Attack ':' AttackDesc                          { Attack $3 }
	     | FullAttack ':' AttackDescList                  { FullAttack $3 }
	     
AttackDescList : AttackDesc                               { [$1] }
	           | AttackDesc ',' AttackDescList            { $1 : $3 }

AttackDesc : AttackRange Modifier DieRoll                 { ($1, $2, $3) }
           | AttackRange DieRoll                          { ($1,  0, $2) }

AttackRange : Melee                                       { Melee }
            | Ranged '(' Nat ')'                          { Ranged $3 }
            
Modifier : '+' Nat                                        { $2 }
         | '-' Nat                                        { - $2 }

DieRoll : DiceExp                                         { DieRoll {dieAmount = fst $1, dieValue = snd $1, modifier = 0} }
        | DiceExp Modifier                                { DieRoll {dieAmount = fst $1, dieValue = snd $1, modifier = $2} }

DiceExp : Nat 'd' Nat                                     { ($1, $3) }
        | Die                                             { $1 }

AIList  : AI Name '{' ActionSeq '}'                       { [($2, $4)] }
        | AI Name '{' ActionSeq '}' AIList                { ($2, $4) : $6 }

ActionSeq : AIAction                                      { $1 }
          | AIAction ActionSeq                            { Cons $1 $2 }

AIAction : TurnDesc ';'                                                   { Turn $1 }
         | If Condition Then '{' ActionSeq '}' Else '{' ActionSeq '}'     { If $2 $5 $9 }
         | While Condition '{' ActionSeq '}'                              { While $2 $4 }


TurnDesc : Pass                                           { [Pass] }
         | MoveAction                                     { [Move $1] }
         | StandardAction                                 { [Standard $1] }
         | FullAction                                     { [Full $1] }
         | MoveAction '-' MoveAction                      { [Move $1, Move $3] }
         | MoveAction '-' StandardAction                  { [Move $1, Standard $3] }
         | StandardAction '-' MoveAction                  { [Standard $1, Move $3] }

MoveAction : Approach Target                              { Approach $2 }
           | Defend                                       { Defend }

StandardAction : Attack Target                            { AttackAction $2 }
 
FullAction : FullAttack Target                            { FullAttackAction $2 }

Target : Self                                             { Self }
       | Name ':' Name ':' Name                           { Specific $1 $3 $5 }
       | Adjective UnitDesc                               { Description $1 $2 }
       
Adjective : Closest                                       { Closest }
          | Furthest                                      { Furthest } 
          | Most Injured                                  { MostInjured }
          | Least Injured                                 { LeastInjured }
          | Last                                          { Last }
       
UnitDesc : Ally                                           { Ally }
         | Enemy                                          { Enemy }
         | Name                                           { Team $1 }
         | Name ':' Name                                  { TeamUnit $1 $3 }

Condition : UnitDesc Count IntComparison                             { UnitCount $1 $3 }
          | UnitDesc In Range '(' RangeExp ')'                       { UnitRange $1 $5 }
          | Name ':' Name ':' Name In Range '(' RangeExp ')'         { SpecificUnitRange $1 $3 $5 $9 }
          | UnitDesc Count In Range '(' RangeExp ')' IntComparison   { UnitRangeCount $1 $6 $8 }
          | Total Turn Count IntComparison                           { TotalTurn $4 }
          | Not Condition                                            { Not $2 }
          | Condition And Condition                                  { And $1 $3 }
          | Condition Or Condition                                   { Or $1 $3 }
          | '(' Condition ')'                                        { $2 }
                    
IntComparison : '>' Int                                   { Comparison (> $2) }
              | '>' '=' Int                               { Comparison (>= $3) }
              | '<' Int                                   { Comparison (< $2) }
              | '<' '=' Int                               { Comparison (<= $3) }
              | '=' Int                                   { Comparison (== $2) }
              | '!' '=' Int                               { Comparison (/= $3) }

RangeExp : Nat                                            { IntR $1 }
         | Melee                                          { MeleeR }
         | Attack                                         { AttackR }
         | FullAttack                                     { FullAttackR }
         | Speed                                          { SpeedR }
         | RangeExp '+' RangeExp                          { Sum $1 $3 }
         | RangeExp '*' Nat                               { Prod $1 $3 }
         | '(' RangeExp ')'                               { $2 }


TeamList : Team Name '{' TeamMemberList '}'               { [($2, $4)] }
         | Team Name '{' TeamMemberList '}' TeamList      { ($2, $4) : $6 }

TeamMemberList : TeamMember ';'                           { [$1] }
               | TeamMember ';' TeamMemberList            { $1 : $3 } 

TeamMember : Name ',' Name Mult ':' CoordinateList        { ($1, $3, $4, $6) }

CoordinateList : '(' Int ',' Int ')'                      { [($2, $4)] }
               | '(' Int ',' Int ')' ',' CoordinateList   { ($2, $4) : $7 }  


{

parseError :: [LexerToken] -> a
parseError ls = error ("Parse error" ++ show ls)

}


 


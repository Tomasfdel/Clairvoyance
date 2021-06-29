{
module Commands.Parser where

import Commands.Tokens
import Commands.Types

}

%name parse
%tokentype { LexerToken }
%error { parseError }

%token
	Next           { LTNext }
	Move           { LTMove }
	Up             { LTUp }
	UpRight        { LTUpRight }
	Right          { LTRight }
	DownRight      { LTDownRight }
	Down           { LTDown }
	DownLeft       { LTDownLeft }
	Left           { LTLeft }
	UpLeft         { LTUpLeft }
	Attack         { LTAttack }
	Kill           { LTKill }
    Nat            { LTNat $$ }
    ':'            { LTSym ':' }
    '-'            { LTSym '-' }
    '('            { LTSym '(' }
    ')'            { LTSym ')' }
    ','            { LTSym ',' }
    Name           { LTVar $$ }
    
%%

Command : Next                                { Next }
        | Move Target Movement                { Move $2 $3 }
        | Attack Target Nat Nat               { Attack $2 $3 $4 }
        | Kill Target                         { Kill $2 }

Target : Nat                                  { Index $1 }
       | Name ':' Name ':' Nat                { Description $1 $3 $5 }
       
Movement : '(' Nat ',' Nat ')'                { Position ($2, $4) }
         | DirectionList                      { Path $1 }
         
DirectionList : Direction                     { [$1] }
              | Direction '-' DirectionList   { $1 : $3 }

Direction : Up                                { DirUp }
          | UpRight                           { DirUpRight }
          | Right                             { DirRight }
          | DownRight                         { DirDownRight }
          | Down                              { DirDown }
          | DownLeft                          { DirDownLeft }
          | Left                              { DirLeft }
          | UpLeft                            { DirUpLeft }


{

parseError :: [LexerToken] -> a
parseError ls = error ("Parse error" ++ show ls)

}


 


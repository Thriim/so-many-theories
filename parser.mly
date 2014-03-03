


%{

  open Ast

%}


%token <int> INTEGER
%token CNF P
%token EQ NEQ EOF EOL


%start file
%type <Ast.cnf> file

%%

file:
| P CNF INTEGER INTEGER EOL clauses EOF { $3, $4, $6 }
;


clauses :
| clause EOL clauses { $1 :: $3 }
| clause EOL { [$1] }
| clause { [$1] }
;


clause :
| equation clause { $1 :: $2 }
| equation { [$1] }
;
      
    

equation :
| INTEGER EQ INTEGER {
  Eq (min $1 $3, max $1 $3) } 
| INTEGER NEQ INTEGER { Neq (min $1 $3, max $1 $3) }
;

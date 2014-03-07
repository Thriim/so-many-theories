


%{

  open Ast
  open Equality_ast

%}


%token <int> INTEGER
%token CNF P
%token EQ NEQ EOF EOL EOC END


%start file sat
%type <Equality_ast.equation Ast.cnf> file
%type <int Ast.cnf> sat

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

/* Boolean parser */

sat:
|  P CNF INTEGER INTEGER EOL bclauses EOF { ($3, $4, $6) }
;

bclauses:
| bclause EOL bclauses { $1 :: $3 }
| bclause EOL { [$1] }
;

bclause:
| INTEGER bclause { $1 :: $2 }
| INTEGER { [$1] }
;




%{

  open Ast
  open Equality_ast

%}


%token <int> INTEGER
%token CNF P
%token EQ NEQ EOF EOL EOC END


%start file sat
%type <Equality_ast.equation Ast.cnf> file
%type <Ast.formula> sat

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

sat:
|  P CNF INTEGER INTEGER EOL bclauses EOF { $6 }
;

bclauses:
| bclause EOL bclauses { Formula.add $1 $3 }
| bclause EOL { Formula.singleton $1 }
;

bclause:
| INTEGER bclause { let v = if $1 < 0 then Not (abs $1) else Var $1 in
                    Clause.add v $2 }
| INTEGER { let v = if $1 < 0 then Not (abs $1) else Var $1 in Clause.singleton v }
;

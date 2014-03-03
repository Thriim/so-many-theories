
type equation =
| Neq of int * int
| Eq of int * int

type clause = equation list

type clauses = clause list

type cnf = int * int * clauses

type sat_var =
| Var of int
| Not of int

type operation = Op of int * int
    
type bcnf = sat_var list list
type system = (operation * int) list * bcnf

val string_of_system : system -> string





type equation =
| Neq of int * int
| Eq of int * int


type clause = equation list

type clauses = clause list

type cnf = int * int * clauses

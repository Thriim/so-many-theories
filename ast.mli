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

module ICMap : Map.S with type key = operation
module Clause : Set.S with type elt = sat_var
module Formula : Set.S with type elt = Clause.t

type formula = Formula.t
type system = int ICMap.t * formula

val not_var : sat_var -> sat_var

val string_of_sat_var : sat_var -> string
val string_of_clause : Clause.t -> string
val string_of_system : system -> string

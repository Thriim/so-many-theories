
type 'a clause = 'a list

type 'a clauses = 'a clause list

type 'a cnf = int * int * 'a clauses

type sat_var =
| Var of int
| Not of int

module IntMap : Map.S with type key = int
module Clause : Set.S with type elt = sat_var
module Formula : Set.S with type elt = Clause.t

type formula = Formula.t
(* type 'a system = (int * int) * 'a IntMap.t * formula *)

type literal = Decision of sat_var | Unit of sat_var * Clause.t
type model = literal list

type 'a env = (int, 'a) Hashtbl.t * ('a, int) Hashtbl.t

type 'a system = (int * int) * 'a env * formula

val not_var : sat_var -> sat_var
val model_to_clause : model -> Clause.t

val string_of_sat_var : sat_var -> string
val string_of_clause : Clause.t -> string
val string_of_system : ('a -> string) -> 'a system -> string
val string_of_model : model -> string

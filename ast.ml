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
module ICMap = Map.Make (struct
  type t = operation
  let compare = compare end)

module Literal = struct
  let compare = Pervasives.compare
  type t = sat_var
end

module Clause = Set.Make (Literal)
module Formula = Set.Make (Clause)

type formula = Formula.t
type system = int ICMap.t * formula

let dummy_map formula =
  let vars =
    Formula.fold (fun cl l ->
      Clause.fold (fun v l ->
          let i = match v with Var i | Not i -> i in
          if List.mem i l then l else i :: l) cl l) formula [] in
  List.fold_left (fun icm i -> ICMap.add (Op (i, i)) i icm) ICMap.empty vars

let not_var = function Not v -> Var v | Var v -> Not v

open Format

let string_of_sat_var = function
  | Not i -> "!" ^ string_of_int i
  | Var i -> string_of_int i

let string_of_system (map, fmla) =
  sprintf "bindings {\n%s}\n%s"
    (ICMap.fold (fun (Op (i1, i2)) v acc ->
      acc ^ (Format.sprintf "%d %d -> %d\n" i1 i2 v)
     ) map "")
    (Formula.fold (fun clause sfml ->
      sprintf "%s\n%s"
      (Clause.fold (fun lit scl ->
        let slit = string_of_sat_var lit in
        sprintf "%s %s" scl slit
       ) clause ("")) sfml
     ) fmla "")

let string_of_clause cl =
  Clause.fold (fun lit scl ->
        let slit = string_of_sat_var lit in
        sprintf "%s %s" scl slit
       ) cl ("")

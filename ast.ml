
type 'a clause = 'a list

type 'a clauses = 'a clause list

type 'a cnf = int * int * 'a clauses

type sat_var =
| Var of int
| Not of int


module IntMap = Map.Make (struct
  type t = int
  let compare = compare end)

module Literal = struct
  let compare = Pervasives.compare
  type t = sat_var
end

module Clause = Set.Make (Literal)
module Formula = Set.Make (Clause)

type formula = Formula.t
(* type 'a system = (int * int) * 'a IntMap.t * formula *)

type literal = Decision of sat_var | Unit of sat_var * Clause.t
type model = literal list


type 'a env = (int, 'a) Hashtbl.t * ('a, int) Hashtbl.t

type 'a system = (int * int) * 'a env * formula

let not_var = function Not v -> Var v | Var v -> Not v

let model_to_clause =
  List.fold_left (fun acc -> function Decision l | Unit (l, _) ->
      Clause.add l acc) Clause.empty

open Format

let string_of_sat_var = function
  | Not i -> "!" ^ string_of_int i
  | Var i -> string_of_int i

let string_of_system f ((_,_), (map, _), fmla) =
  sprintf "bindings {\n%s}\n%s"
    (Hashtbl.fold (fun  v op acc ->
      acc ^ (Format.sprintf "%s -> %d\n" (f op) v)
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


let string_of_model m =
  List.fold_right (fun v acc -> match v with
      | Decision v -> string_of_sat_var v ^ "@ :: " ^ acc
      | Unit (v, _) -> string_of_sat_var v ^ " :: " ^ acc) m "[]"

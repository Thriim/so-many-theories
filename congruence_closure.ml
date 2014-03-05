
open Ast

module RelSet = Set.Make (struct
  type t = int * int
  let compare (a1, b1) (a2, b2) =
    if a1 = a2 && b1 = b2 ||
      a1 = b2 && b1 = a2 then 0
    else 1
end)


let sets_of_system clauses =
  List.fold_left (fun (eq, ineq) clause ->
    List.fold_left (fun (eq, ineq) equation ->
	    match equation with
	    | Neq (i1, i2) -> eq, RelSet.add (i1, i2) ineq
      | Eq (i1, i2) -> RelSet.add (i1, i2) ineq, eq
    ) (eq, ineq) clause
  ) (RelSet.empty, RelSet.empty) clauses

open Union_find


let algorithm h eqs ineqs =
  let rec step h eqs =
    let a, b as e = RelSet.choose eqs in
    let h' = union h a b in
    if find h a = find h b && RelSet.mem e ineqs then false
    else step h' (RelSet.remove e eqs)
  in try step h eqs with Not_found -> true

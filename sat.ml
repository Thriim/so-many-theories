
open Ast

let translate ast =
  let counter = ref 0 in
  let nvar, nclauses, clauses = ast in
  let env, clauses =
    List.fold_left (fun (env, accl) clause ->
    let env, disj = List.fold_left (fun (env, acccl) equation ->
      let f, i1, i2 =
	match equation with
	| Neq (i1, i2) -> (fun x -> Not x), i1, i2
	| Eq (i1, i2) -> (fun x -> Var x), i1, i2
      in
      let var, env = try ICMap.find (Op (i1, i2)) env, env with
	  Not_found ->
	    incr counter; !counter,
	    ICMap.add (Op (i1, i2)) (!counter) env
      in
      env, f var :: acccl
    ) (env, []) clause  in
    env, (List.rev disj) :: accl
  ) (ICMap.empty, []) clauses in
  env, List.rev clauses

type result = Sat | Unsat

type literal = Decision of sat_var | Unit of sat_var
type model = literal list

(** Tests if the variables in [m] are a correct model for [c] *)
let is_model m c =
  (* Boolean tests by hand or using BDDs or whatever that would work! *)
  true

exception Wrong_model

let satisfies m f =
  (* If it returns false once, we don't test for the rest, just returns *)
  try
    List.fold_left
      (fun acc c -> if not (is_model m c) then raise Wrong_model else acc) true f
  with Wrong_model -> false

let solver (env, bcnf) =
  let m = [] in
  (*
     Core algorithm
  *)
  if satisfies m bcnf then Sat else Unsat

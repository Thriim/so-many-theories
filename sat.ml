
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

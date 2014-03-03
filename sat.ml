


open Ast



let translate ast =
  let counter = ref 0 in
  let nvar, nclauses, clauses = ast in
  List.fold_left (fun (env, accl) clause ->
    let disj, env = List.fold_left (fun (acccl, env) equation ->
      let f, i1, i2 = 
	match equation with
	| Neq (i1, i2) -> (fun x -> Not x), i1, i2
	| Eq (i1, i2) -> (fun x -> Var x), i1, i2  
      in
      let var, env = try List.assoc (Op (i1, i2)) env, env with 
	  Not_found ->
	    incr counter; !counter,
	    (Op (i1, i2), !counter) :: env
      in
      f var :: acccl, env
    ) ([], env) clause  in
    env, disj :: accl
  ) ([], []) clauses
  
  

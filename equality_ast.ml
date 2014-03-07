open Ast

type equation =
| Neq of int * int
| Eq of int * int

type operation = Op of int * int

let string_of_operation (Op (i1, i2)) =
  Format.sprintf "%d %d" i1 i2

let string_of_op_system = string_of_system string_of_operation

exception End of int

let translate ast =
  let counter = ref 0 in
  let nvar, nclauses, formula = ast in
  let env, formula =
    List.fold_left (fun (env, accf) clause ->
      let env, disj = List.fold_left (fun (env, acccl) equation ->
        let f, i1, i2 =
	  match equation with
	  | Neq (i1, i2) -> (fun x -> Not x), i1, i2
	  | Eq (i1, i2) -> (fun x -> Var x), i1, i2
        in
        let var, env =
          try
            IntMap.iter (fun k (Op (j1, j2)) ->
              if j1 = i1 && j2 = i2 then raise (End k)) env;
	    incr counter; !counter,
	    IntMap.add (!counter) (Op (i1, i2)) env
          with End k -> k, env
        in
        env, Clause.add (f var) acccl
      ) (env, Clause.empty) clause  in
      env, Formula.add disj accf
    ) (IntMap.empty, Formula.empty) formula in
  (nvar, nclauses), env, formula

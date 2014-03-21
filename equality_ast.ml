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
  let inv_env = Hashtbl.create nvar in
  let env = Hashtbl.create nvar in
  let formula =
    List.fold_left (fun accf clause ->
      let disj = List.fold_left (fun acccl equation ->
        let f, i1, i2 =
	  match equation with
	  | Neq (i1, i2) -> (fun x -> Not x), i1, i2
	  | Eq (i1, i2) -> (fun x -> Var x), i1, i2
        in
        let var =
          try
            Hashtbl.iter (fun k (Op (j1, j2)) ->
              if j1 = i1 && j2 = i2 then raise (End k)) env;
	    incr counter;
	    Hashtbl.add env (!counter) (Op (i1, i2));
            !counter
          with End k -> k
        in
        Clause.add (f var) acccl
      ) Clause.empty clause  in
      Formula.add disj accf
    ) Formula.empty formula in
  Hashtbl.iter (fun k v -> Hashtbl.add inv_env v k) env;
  (nvar, nclauses), (env, inv_env), formula


open Ast

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
        let var, env = try ICMap.find (Op (i1, i2)) env, env with
	          Not_found ->
	            incr counter; !counter,
	            ICMap.add (Op (i1, i2)) (!counter) env
        in
        env, Clause.add (f var) acccl
      ) (env, Clause.empty) clause  in
      env, Formula.add disj accf
    ) (ICMap.empty, Formula.empty) formula in
  env, formula

type result = Sat | Unsat

type literal = Decision of sat_var | Unit of sat_var
type model = literal list

(** Tests if the variables in [m] are a correct model for [c] *)
let is_model m c = Clause.for_all
    (fun v -> List.mem (Decision v) m || List.mem (Unit v) m) c

(** Checks that the model satisfies each clause in the formula *)
let satisfies m f = Formula.for_all (is_model m) f

let contains_decision_literal m =
  List.exists (function Decision _ -> true | _ -> false) m

let solver (env, bcnf) =
  let m = [] in
  let time = ref 0 in
  let vsids_cst = 3 in
  let find_two_literals bcnf = () in
  let rec step m f vars =

    (* VSIDS *)
    incr time;
    let vars = if !time mod 10 = 0 then
        List.map (fun (v, x) -> v, x / vsids_cst (* ? *)) vars
      else vars in

    if satisfies m bcnf then Sat
    else if contains_decision_literal m then
      (* Backtrack *)
      step m f vars
    else
      (* Try unit and call step *)
      (* else try decision and call step *)
      (* else *)
      Unsat
  in
  step m bcnf []

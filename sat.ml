
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
let is_model m c = List.for_all (fun v ->

  true) c
(* exception Wrong_model *)

let satisfies m f = List.for_all (is_model m) f  (* => is that okay too ? *)
  (* If it returns false once, we don't test for the rest, just returns *)

  (* try *)
  (*   List.fold_left *)
  (*     (fun acc c -> if not (is_model m c) then raise Wrong_model else acc) true f *)
  (* with Wrong_model -> false *)

let contains_decision_literal m =
  List.exists (function Decision _ -> true | _ -> false) m

(* Maybe we should do CDCL instead of DPLL ? *)
let solver (env, bcnf) =
  let m = [] in
  let time = ref 0 in
  let rec step m f vars =

    (* VSIDS *)
    incr time;
    let vars = if !time mod 10 = 0 then
        List.map (fun (v, x) -> v, x / 3 (* ? *)) vars
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
  step m bcnf

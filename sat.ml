
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

type literal = Decision of sat_var | Unit of sat_var
type model = literal list

let var = function Not v | Var v -> Var v

let not_var = function Not v -> Var v | Var v -> Not v

(** Tests if the variables in [m] are a correct model for [c] *)
let is_model m c = Clause.exists
    (fun v -> List.mem (Decision v) m || List.mem (Unit v) m) c

(** Checks that the model satisfies each clause in the formula *)
let satisfies m f = Formula.for_all (is_model m) f

(** Returns true if there exists a clause that can be false in the model,
  i.e. its literal are "false" in the model *)
let can_unsat m c = Clause.for_all
    (fun v -> List.mem (Decision (not_var v)) m
              || List.mem (Unit (not_var v)) m) c

(** Returns true if there is a literal with the actual model that can be unsat *)
let is_unsat m f = Formula.exists (can_unsat m) f

let contains_decision_literal m =
  List.exists (function Decision _ -> true | _ -> false) m

(** Splits the model for the backtrack rule, returning the variables to add to
    the free pool *)
let split_at_decision m =
  let rec step m1 = function
  | [] -> assert false
  | Unit v :: tl -> step (Clause.add (var v) m1) tl
  | Decision v :: tl ->
    let v = match v with
      | Var v -> Not v
      | Not v -> Var v in
    m1, Unit v :: tl in
  step Clause.empty m

(** Applies the literals in the model to the clause, returning those with no
  value and those that matches *)
let apply m c =
  let value c = match c with Decision c | Unit c -> c in
  let m = List.map value m in
  Clause.partition (fun v -> List.mem v m) c

exception Unsat
exception No_literal

let string_of_model m =
  List.fold_right (fun v acc -> match v with
      | Decision v -> string_of_sat_var v ^ "@ :: " ^ acc
      | Unit v -> string_of_sat_var v ^ " :: " ^ acc) m "[]"

(** Takes a variable in the pool and add it as Decision literal, or raises
  No_literal if no literal free *)
let decision m f pool =
  let l = try Clause.choose pool
    with Not_found -> raise No_literal in Decision l :: m, Clause.remove l pool

(** Naive unit rule, which looks in EVERY clause to find a literal *)
let unit m f pool =
  let l, cl = Formula.fold (fun cl acc ->
      let l', cl' = apply m cl in
      if Clause.cardinal l' = 1 then
        let l' = Clause.choose l' in
        if not (List.exists (function Decision v | Unit v -> v = l') m) then l', cl'
        else acc
      else acc)
      f (Var (-1), Clause.empty) in
  if l = Var (-1) then raise No_literal else Unit l :: m, Clause.remove l pool


let solver (env, bcnf) =
  let m = [] in
  (* let time = ref 0 in *)
  (* let vsids_cst = 3 in *)
  (* let find_two_literals bcnf = () in *)

  let rec step m f vars pool =

    (* (\* VSIDS *\) *)
    (* incr time; *)
    (* let vars = if !time mod 10 = 0 then *)
    (*     List.map (fun (v, x) -> v, x / vsids_cst (\* ? *\)) vars *)
    (*   else vars in *)
    try
      if satisfies m f then m
      else if is_unsat m f then raise Unsat
      else
        let m, pool =
          try unit m f pool
          with No_literal ->
            try decision m f pool
            with No_literal -> raise Unsat in
        step m f vars pool
    with Unsat ->
      let m1, m2 = split_at_decision m in
      step m2 f vars (Clause.union m1 pool)

  in
  let pool = ICMap.fold (fun _ i pool -> Clause.add (Var i) pool) env Clause.empty in
  step m bcnf [] pool


open Ast


let var = function Not v | Var v -> Var v

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
  let cl, l = Clause.partition (fun v -> List.mem (not_var v) m) c in
  l, cl

let backjump_split m cl =
  let rec step prev m =
    Format.printf "Actual model: %s@." @@ string_of_model m;
    if not (contains_decision_literal m) then prev, m
    else
      let m1, m2 = split_at_decision m in
      let l', cl' = apply m2 cl in
      if Clause.cardinal l' = 1 then
        let l = not_var (Clause.choose l') in
        Clause.union prev m1, Unit l :: (List.tl m2)
      else step (Clause.union prev m1) m2
  in
  step Clause.empty m

exception Unsat
exception No_literal


module type TheorySolver =
  sig
    type t
    type repr
    type predicate
    val empty : repr Ast.system -> t
    val translate : predicate Ast.cnf -> repr Ast.system
    val add_literal : repr IntMap.t -> sat_var -> t -> t option
  end

module Boolean = struct
  open Equality_ast

  type t = unit
  type repr = int
  type predicate = int
  let empty _ = ()

  let translate (nv ,nc, cnf) =
    let env, f = List.fold_left (fun (m, f) cl ->
        let m, cl = List.fold_left (fun (m, cl) v ->
            let v' = if v < 0 then Not (abs v) else Var v in
            IntMap.add (abs v) (abs v) m, Clause.add v' cl) (m, Clause.empty) cl in
        m, Formula.add cl f) (IntMap.empty, Formula.empty) cnf in
    ((nv, nc), env, f)

  let add_literal _ _ t = Some t
end

module Make =
  functor(T : TheorySolver) ->
  struct

    type solver_model = {
      env : T.repr IntMap.t;
      formula : formula;
      model : model;
      pool : Clause.t;
      theory : T.t;
      previous : T.t list
    }

    type result = Continue of solver_model | Backtrack of solver_model

    (** Takes a variable in the pool and add it as Decision literal, or raises
        No_literal if no literal free *)
    let decision m =
      let l = try Clause.choose m.pool
        with Not_found -> raise No_literal in
      let prev = m.theory in
      let theory, f = match T.add_literal m.env l m.theory with
        | Some h -> h, (fun m -> Continue m)
        | None -> m.theory, (fun m -> Backtrack m) in
      f { m with
        model = Decision l :: m.model;
        pool = Clause.remove l m.pool;
        previous = prev :: m.previous;
        theory
      }

    exception Found of sat_var * Clause.t

    (** Naive unit rule, which find the first clause that have only one unresolved variable *)
    let unit m =
      let l, cl =
        try Formula.fold (fun cl acc ->
            let l', cl' = apply m.model cl in
            if Clause.cardinal l' = 1 then
              let l' = Clause.choose l' in
              if not (List.exists (function Decision v | Unit v -> v = l') m.model)
              then raise (Found (l', cl'))
              else acc
            else acc)
            m.formula (Var (-1), Clause.empty)
        with Found (l, cl) -> l, cl in
      if l = Var (-1) then raise No_literal
      else
        let theory, f = match T.add_literal m.env l m.theory with
        | Some h -> h, (fun m -> Continue m)
        | None -> m.theory, (fun m -> Backtrack m) in
        f { m with
          model = Unit l :: m.model;
          pool = Clause.remove l m.pool;
          theory
        }

    let backtrack m =
      let m1, m2 = split_at_decision m.model in
      let theory, previous = List.hd m.previous, List.tl m.previous in
      Continue { m with model = m2; pool = Clause.union m1 m.pool; theory; previous }

    let solver ((nv, nc), env, bcnf) =
      let system = ((nv, nc), env, bcnf) in
      (* let time = ref 0 in *)
      (* let vsids_cst = 3 in *)
      (* let find_two_literals bcnf = () in *)

      let rec step m =

        (* (\* VSIDS *\) *)
        (* incr time; *)
        (* let vars = if !time mod 10 = 0 then *)
        (*     List.map (fun (v, x) -> v, x / vsids_cst (\* ? *\)) vars *)
        (*   else vars in *)

        let do_backtrack, m = match m with
          | Continue m -> false, m
          | Backtrack m -> true, m in

        if satisfies m.model m.formula && not do_backtrack then m.model
        else if is_unsat m.model m.formula || do_backtrack then
          if not (contains_decision_literal m.model) then raise Unsat
          else
            (* let clause = cut gr in *)
            (* Format.printf "Graph: %s@." @@ string_of_igraph gr; *)
            (* let m1, m2 = backjump_split m clause in *)
            (* let pool = (Clause.union m1 pool) in *)
            (* let gr = empty pool in *)
            (* print_endline "Backjump success"; *)
            step @@ backtrack m
        else
          let m =
            try unit m
            with No_literal ->
              try decision m
              with No_literal -> Backtrack m in
          step m

      in
      let pool = IntMap.fold (fun i _ pool -> Clause.add (Var i) pool) env Clause.empty in
      step (Continue { model = [];
             env;
             formula = bcnf;
             pool;
             theory = T.empty @@ system;
             previous = [] })

  end

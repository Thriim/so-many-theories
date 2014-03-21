
open Ast


type algorithm = CDCL | DPLL


let var = function Not v | Var v -> Var v

(** Tests if the variables in [m] are a correct model for [c] *)
let is_model m c = Clause.exists
    (fun v -> List.exists (function Decision l | Unit (l, _) -> v = l) m) c

(** Checks that the model satisfies each clause in the formula *)
let satisfies m f = Formula.for_all (is_model m) f

(** Returns true if there exists a clause that can be false in the model,
  i.e. its literal are "false" in the model *)
let can_unsat m c = Clause.for_all
    (fun v ->
       List.exists (function Decision l | Unit (l, _) -> not_var l = v) m) c

(** Returns true if there is a literal with the actual model that can be unsat *)
let is_unsat m f = Formula.exists (can_unsat m) f

(** Returns every clauses that are unsatisfiable *)
let unsatisfiable_clauses m f = Formula.filter (can_unsat m) f

let contains_decision_literal m =
  List.exists (function Decision _ -> true | _ -> false) m

(** Splits the model for the backtrack rule, returning the variables to add to
    the free pool *)
let split_at_decision m =
  let rec step m1 = function
  | [] -> assert false
  | Unit (v, _) :: tl -> step (Clause.add (var v) m1) tl
  | Decision v :: tl ->
    let v = match v with
      | Var v -> Not v
      | Not v -> Var v in
    m1, Unit (v, Clause.empty) :: tl in
  step Clause.empty m

(** Applies the literals in the model to the clause, returning those with no
  value and those that matches *)
let apply m c =
  let value c = match c with Decision c | Unit (c, _) -> c in
  let m = List.map value m in
  let cl, l = Clause.partition (fun v -> List.mem (not_var v) m) c in
  l, cl

let in_m v m =
  List.exists (function
      | Decision c | Unit (c, _) -> c = v || not_var v = c) m


exception Unsat

module type TheorySolver =
  sig
    type t
    type repr
    type predicate
    val empty : repr Ast.system -> t
    val translate : predicate Ast.cnf -> repr Ast.system
    val add_literal : repr env -> sat_var -> t -> t option
    val propagate : repr env -> model -> t -> model
  end

module Boolean = struct
  open Equality_ast

  type t = unit
  type repr = int
  type predicate = int
  let empty _ = ()

  let translate (nv ,nc, cnf) =
    let env = Hashtbl.create nv in
    let inv_env = Hashtbl.create nv in
    let f = List.fold_left (fun f cl ->
        let cl = List.fold_left (fun cl v ->
            let v' = if v < 0 then Not (abs v) else Var v in
            Hashtbl.add env (abs v) (abs v);
            Clause.add v' cl) Clause.empty cl in
        Formula.add cl f) Formula.empty cnf in
    ((nv, nc), (env, inv_env), f)

  let add_literal _ _ t = Some t
  let propagate _ m _ = m
end

module Make =
  functor(T : TheorySolver) ->
  struct

    type mode = Resolution | Search

    exception No_literal

    type solver_model = {
      env : T.repr env;
      formula : formula;
      model : model;
      pool : Clause.t;
      theory : T.t;
      previous : T.t list;
      mode : mode;
      resolved : Clause.t;
      vsids : (int, int) Hashtbl.t
    }

    type result = Continue of solver_model | Backtrack of solver_model

    (** Takes a variable in the pool and add it as Decision literal, or raises
        No_literal if no literal free *)
    let decision_dpll m =
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

    let decision_cdcl m =
      let l = try Clause.choose m.pool
        with Not_found -> raise No_literal in
      let prev = m.theory in
      (* Format.printf "Adding decision l: %s@." @@ string_of_sat_var l; *)
      let theory, mode, resolved = match T.add_literal m.env l m.theory with
        | Some h -> h, Search, m.resolved
        | None -> m.theory, Resolution,
                  List.fold_left (fun acc ->
                      function Decision l | Unit (l, _) ->
                        Clause.add (not_var l) acc)
                    Clause.empty m.model in
      { m with
        model = Decision l :: m.model;
        pool = Clause.remove l m.pool;
        previous = prev :: m.previous;
        theory; mode; resolved
      }


    exception Found of sat_var * Clause.t

    let unit_search m =
      try Formula.fold (fun cl acc ->
          let l', cl' = apply m.model cl in
          if Clause.cardinal l' = 1 then
            let l' = Clause.choose l' in
            if not (List.exists
                      (function Decision v | Unit (v, _)
                        -> v = l' || v = not_var l') m.model)
            then raise (Found (l', cl'))
            else acc
          else acc)
          m.formula (Var 0, Clause.empty)
      with Found (l, cl) -> l, cl


    (** Naive unit rule, which find the first clause that have only one unresolved variable *)
    let unit_dpll m =
      let l, cl = unit_search m in
      if l = Var 0 then raise No_literal
      else
        let theory, f = match T.add_literal m.env l m.theory with
          | Some h -> h, (fun m -> Continue m)
          | None -> m.theory, (fun m -> Backtrack m) in
        f { m with
            model = Unit (l, Clause.add l cl) :: m.model;
            pool = Clause.remove l m.pool;
            theory
          }

    let unit_cdcl m =
      let l, cl = unit_search m in
      if l = Var 0 then raise No_literal
      else begin
        (* Format.printf "Unit adding l: %s@." @@ string_of_sat_var l; *)
        let prev = m.theory in
        let theory, mode, resolved = match T.add_literal m.env l m.theory with
          | Some h -> h, Search, m.resolved
          | None -> m.theory, Resolution, List.fold_left (fun acc ->
                      function Decision l | Unit (l, _) ->
                        Clause.add (not_var l) acc)
                    Clause.empty m.model in
        { m with
          model = Unit (l, Clause.add l cl) :: m.model;
          pool = Clause.remove (var l) m.pool;
          theory; previous = prev :: m.previous; mode; resolved
        }
      end

    let backtrack m =
      let m1, m2 = split_at_decision m.model in
      let theory, previous = List.hd m.previous, List.tl m.previous in
      Continue { m with model = m2; pool = Clause.union m1 m.pool; theory; previous }


    exception Cannot_backjump

    let split m =
      let rec step acc pool model ths =
        if not (contains_decision_literal model) then acc
        else begin
          let m1, m2 = split_at_decision model in
          let th, prev_ths = List.hd ths, List.tl ths in
          let ld, m2 = List.hd m2, List.tl m2 in
          let ld = match ld with
            | Decision v | Unit (v, _) -> var v in
          let pool = Clause.add ld @@ Clause.union pool m1 in
          let l', cl' = apply m2 m.resolved in
          let acc =
            if Clause.cardinal l' = 1 then
              let l = Clause.choose l' in
              if Clause.mem (var l) pool then begin
                Some ((Clause.remove (var l) pool,
                       Unit (l, m.resolved) :: m2,
                       th, prev_ths)) end
              else acc
            else acc in
          step acc pool m2 prev_ths end in
      match step None m.pool m.model m.previous with
      | None -> raise Cannot_backjump
      | Some (pool, model, theory, previous) -> (pool, model, theory, previous)


    let backjump m =
      let pool, model, theory, previous = split m in
      { m with model; theory; previous; pool;
               mode = Search;
               formula = Formula.add m.resolved m.formula;
               resolved = Clause.empty }

    exception No_conflict

    let conflict m =
      try
        let cl = Formula.choose @@ unsatisfiable_clauses m.model m.formula in
        { m with mode = Resolution; resolved = cl }
      with Not_found -> raise No_conflict

    (** When mode = Search *)

    exception Cannot_resolve

    let resolve m =
      let v, cl = List.fold_left (fun acc l -> match l with
          | Decision _ -> acc
          | Unit (v, cl) ->
            if Clause.mem (not_var v) m.resolved then (v, Clause.remove v cl)
            else acc) (Var (-1), Clause.empty) m.model in
      if v = Var (-1) then raise Cannot_resolve
      else
        let r = Clause.union cl @@ Clause.remove (not_var v) m.resolved in
        { m with resolved = r }

    let propagate m =
      let model = T.propagate m.env m.model m.theory in
      { m with model }

    let cdcl ((nv, nc), env, bcnf) =
      let system = ((nv, nc), env, bcnf) in
      let pool = Hashtbl.fold (fun i _ pool -> Clause.add (Var i) pool) (fst env) Clause.empty in
      let start = { model = [];
                    env;
                    formula = bcnf;
                    pool;
                    theory = T.empty system;
                    previous = [];
                    mode = Search;
                    resolved = Clause.empty;
                    vsids = Hashtbl.create nv;
                  } in
      Clause.iter (function Var v | Not v -> Hashtbl.add start.vsids v 0) start.pool;

      let rec step m =
        (* Format.printf "Actual model: %s\nresolved: %s@." *)
        (*   (string_of_model m.model) (string_of_clause m.resolved); *)
        match m.mode with
        | Search ->
          if satisfies m.model m.formula then m.model
          else
            let m = propagate m in
            let m =
              try conflict m
              with No_conflict ->
                try unit_cdcl m
                with No_literal ->
                  try decision_cdcl m
                  with No_literal -> raise Unsat in
            step m
        | Resolution ->
          if not (contains_decision_literal m.model) then raise Unsat
          else
            let m =
              try resolve m
              with Cannot_resolve ->
                try backjump m
                with Cannot_backjump ->
                  raise Unsat in
            step m in
      step start


    let dpll ((nv, nc), env, bcnf) =
      let system = ((nv, nc), env, bcnf) in
      let pool = Hashtbl.fold (fun i _ pool -> Clause.add (Var i) pool) (fst env) Clause.empty in
      let start = { model = [];
                    env;
                    formula = bcnf;
                    pool;
                    theory = T.empty system;
                    previous = [];
                    mode = Search;
                    resolved = Clause.empty;
                    vsids = Hashtbl.create nv
                  } in
      Clause.iter (function Var v | Not v -> Hashtbl.add start.vsids v 0) start.pool;

      let rec step m =

        let do_backtrack, m = match m with
          | Continue m -> false, m
          | Backtrack m -> true, m in

        if satisfies m.model m.formula && not do_backtrack then m.model
        else if is_unsat m.model m.formula || do_backtrack then
          if not (contains_decision_literal m.model) then raise Unsat
          else
            step @@ backtrack m
        else
          let m =
            try unit_dpll m
            with No_literal ->
              try decision_dpll m
              with No_literal -> Backtrack m in
          step m

      in
      step (Continue start)

    let solver = function
      | CDCL -> cdcl
      | DPLL -> dpll

  end

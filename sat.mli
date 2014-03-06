
open Ast

exception Unsat
exception No_literal

val translate : Ast.cnf -> Ast.system

module type TheorySolver =
  sig
    type t
    val empty : 'a -> t
    val add_literal : operation IntMap.t -> sat_var -> t -> t option
    (* val is_coherent : operation IntMap.t -> model -> t -> bool *)
  end

module Boolean :
sig
    type t
    val empty : 'a -> t
    val add_literal : operation IntMap.t -> sat_var -> t -> t option
    (* val is_coherent : operation IntMap.t -> model -> t -> bool *)
end

module Make : functor (T : TheorySolver) ->
  sig

    type solver_model = {
      env : operation IntMap.t;
      formula : formula;
      model : model;
      pool : Clause.t;
      theory : T.t;
      previous : T.t list
    }

    val solver : Ast.system -> model

  end

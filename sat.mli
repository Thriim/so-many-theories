open Ast

exception Unsat
exception No_literal

val dummy_map : formula -> Equality_ast.operation IntMap.t

module type TheorySolver =
  sig
    type t
    type repr
    type input
    val empty : int -> t
    val translate : input Ast.cnf -> repr Ast.system
    val add_literal : repr IntMap.t -> sat_var -> t -> t option
  end

module Boolean :
sig
    type t = unit
    type repr = Equality_ast.operation
    type input = Equality_ast.equation
    val empty : int -> t
    val translate : input Ast.cnf -> repr Ast.system
    val add_literal : repr IntMap.t -> sat_var -> t -> t option
end

module Make : functor (T : TheorySolver) ->
  sig

    type solver_model = {
      env : T.repr IntMap.t;
      formula : formula;
      model : model;
      pool : Clause.t;
      theory : T.t;
      previous : T.t list
    }

    val solver : T.repr Ast.system -> model

  end


open Ast

exception Unsat
exception No_literal


type literal = Decision of Ast.sat_var | Unit of Ast.sat_var


type model = literal list


val translate : Ast.cnf -> Ast.system

val string_of_model : model -> string


module type TheorySolver =
  sig
    type t
    val empty : 'a -> t
    val add_literal : int ICMap.t -> sat_var -> t -> t
    val is_coherent : int ICMap.t -> model -> t -> bool
  end

module Boolean :
sig
    type t
    val empty : 'a -> t
    val add_literal : int ICMap.t -> sat_var -> t -> t
    val is_coherent : int ICMap.t -> model -> t -> bool
end

module Make : functor (T : TheorySolver) ->
  sig

    type solver_model = {
      env : int ICMap.t;
      formula : formula;
      model : model;
      pool : Clause.t;
      theory : T.t;
      previous : T.t list
    }

    val solver : Ast.system -> model

  end

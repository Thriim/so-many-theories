open Ast

exception Unsat

type algorithm = CDCL | DPLL

(** The signature that allows the solver to reason about a theory *)
module type TheorySolver =
  sig

    (** The type of the theory solver data structure *)
    type t

    (** Its representation, for example an operation in the equality case *)
    type repr

    (** Used by translate, that takes a cnf with predicates of the theory in it *)
    type predicate

    (** Creates an empty theory solver structure from the number of unique
        variables in this theory *)
    val empty : repr Ast.system -> t

    (** Translates the cnf into a usable set of clauses and its mapping
        environment *)
    val translate : predicate Ast.cnf -> repr Ast.system

    (** Add a literal in the theorySolver, returns None if it renders it
      inconsistent, or Some otherwise *)
    val add_literal : repr env -> sat_var -> t -> t option

    (** Adds if possible a literal that is implied by the model *)
    val propagate : repr env -> model -> t -> model
  end

module Boolean :
sig
    type t = unit
    type repr = int
    type predicate = int
    val empty : repr Ast.system -> t
    val translate : predicate Ast.cnf -> repr Ast.system
    val add_literal : repr env -> sat_var -> t -> t option
    val propagate : repr env -> model -> t -> model
end

module Make : functor (T : TheorySolver) ->
  sig

    type mode = Resolution | Search

    type solver_model = {
      env : T.repr env; (** the mapping from propositional variables to
                               theory predicates *)
      formula : formula; (** The original formula *)
      model : model; (** The model created by the solver during its execution *)
      pool : Clause.t; (** Variables unused in the model *)
      theory : T.t; (** The theory solver structure during the execution *)
      previous : T.t list; (** The previous theory solvers, used for backtracking *)
      mode : mode;
      resolved : Clause.t;
      vsids : (int, int) Hashtbl.t
    }

    val solver : algorithm -> T.repr Ast.system -> model

  end

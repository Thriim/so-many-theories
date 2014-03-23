open Ast

(** An equation is either an equality or a disequality. It is the result of the
   parsing. *)
type equation =
| Neq of int * int
| Eq of int * int

(** The operation is the representation by the solver *)
type operation = Op of int * int

val string_of_operation : operation -> string

val string_of_op_system : operation system -> string

val translate : equation Ast.cnf -> operation system

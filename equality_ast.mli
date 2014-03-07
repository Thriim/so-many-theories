open Ast

type equation =
| Neq of int * int
| Eq of int * int

type operation = Op of int * int

val string_of_operation : operation -> string

val string_of_op_system : operation system -> string

val translate : equation Ast.cnf -> operation system

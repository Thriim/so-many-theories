


exception Unsat
exception No_literal

val solver : Ast.system -> Ast.model
val translate : Ast.cnf -> Ast.system

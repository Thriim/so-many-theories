


exception Unsat
exception No_literal


type literal = Decision of Ast.sat_var | Unit of Ast.sat_var
type model = literal list


val solver : Ast.system -> model
val translate : Ast.cnf -> Ast.system


val string_of_model : model -> string

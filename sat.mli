


type result = Sat | Unsat

type literal = Decision of Ast.sat_var | Unit of Ast.sat_var
type model = literal list


val solver : Ast.system -> result
val translate : Ast.cnf -> Ast.system

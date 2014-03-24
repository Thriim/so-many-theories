


open Ast
open Equality_ast

module RelSet = Set.Make (struct
  type t = int * int
  let compare (a1, b1) (a2, b2) =
    if a1 = a2 && b1 = b2 ||
      a1 = b2 && b1 = a2 then 0
    else 1
end)

open Union_find

type error = UnboundPropVar of int
exception Error of error


let increment_eqs h ineqs (Op (a, b)) =
  let ra = find h a in
  let rb = find h b in
  let h' = union h ra rb in
  if ra = rb && RelSet.mem (a, b) ineqs then None
  else Some (h', ineqs)

let increment_ineqs h ineqs (Op (a, b)) =
  let ra = find h a in
  let rb = find h b in
  if ra = rb then None
  else Some (h, RelSet.add (ra, rb) ineqs)

exception End of int


let find_op (_, optbl) op = Hashtbl.find optbl op
let find_int (inttbl, _) i = Hashtbl.find inttbl i



let op a b = let a, b = if a < b then a, b else b, a in Op (a, b)


let rec propagate : operation Ast.env -> Ast.model -> (Union_find.t * RelSet.t) -> Ast.model = fun ((optbl, inttbl) as env) model (h, ineqs) ->
  let add_one op m =
    try
      let i = find_op env op in
      let lit = Decision (Var i) in
      if (List.exists (fun lit ->
          let var = match lit with
            | Decision v -> v | Unit (v, _)  -> v
          in
          match var with
          | Not i' -> assert false
          | Var i' -> i' = i
        ) m)
      then m else lit :: m
    with Not_found -> m
  in

  let add_model m lit =
    let var = match lit with
      | Decision v -> v | Unit (v, _)  -> v
    in
    match var with
    | Not i -> m
    | Var i ->
      let Op(a, b) = begin try find_int env i with
      | Not_found -> raise (Error (UnboundPropVar i))
      end in
      let ra, rb = find h a, find h b in
      if ra = rb then
        (if ra <> a then
            if rb <> b then
              add_one (op ra b) (add_one (op rb a) m)
            else add_one (op ra b) m
         else (if rb <> b then add_one (op rb a) m else m))
      else assert false

  in
  List.fold_left add_model model model




module Solver = struct
  type t = Union_find.t * RelSet.t
  type repr = operation
  type predicate = equation

  let translate = translate

  let empty ((nv, _), _, _) = Union_find.create (nv + 1), RelSet.empty

  let add_literal env var (h, ineqs) =
    let i, f = begin match var with
    | Var i -> i, increment_eqs h ineqs
    | Not i -> i, increment_ineqs h ineqs
    end in
    let op = begin try Hashtbl.find (fst env) i with
      Not_found -> raise (Error (UnboundPropVar i))
    end in f op

  let propagate = propagate
end

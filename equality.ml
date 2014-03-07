


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

let increment_eqs h (Op (a, b)) ineqs =
  let ra = find h a in
  let rb = find h b in
  let h' = union h ra rb in
  if ra = rb && RelSet.mem (a, b) ineqs then None
  else Some (h', ineqs)

let increment_ineqs h (Op (a, b)) ineqs =
  let ra = find h a in
  let rb = find h b in
  if ra = rb then None
  else Some (h, RelSet.add (ra, rb) ineqs)

exception End of int


module Solver = struct
  type t = Union_find.t * RelSet.t
  type repr = operation
  type input = equation

  let translate = translate

  let empty n = Union_find.create (n + 1), RelSet.empty

  let add_literal env var (h, ineqs) =
    let i, f = begin match var with
    | Var i -> i, (fun op -> increment_eqs h op ineqs)
    | Not i -> i, (fun op -> increment_ineqs h op ineqs)
    end in
    let op = begin try IntMap.find i env with
      Not_found -> raise (Error (UnboundPropVar i))
    end in f op

end

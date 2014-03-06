


open Ast

module RelSet = Set.Make (struct
  type t = int * int
  let compare (a1, b1) (a2, b2) =
    if a1 = a2 && b1 = b2 ||
      a1 = b2 && b1 = a2 then 0
    else 1
end)

let sets_of_clauses clauses =
  List.fold_left (fun (eq, ineq) clause ->
    List.fold_left (fun (eq, ineq) equation ->
	    match equation with
	    | Neq (i1, i2) -> eq, RelSet.add (i1, i2) ineq
      | Eq (i1, i2) -> RelSet.add (i1, i2) ineq, eq
    ) (eq, ineq) clause
  ) (RelSet.empty, RelSet.empty) clauses

open Union_find

type error = UnboundPropVar of int
exception Error of error
    
let increment h (Op (a, b)) ineqs =
  let h' = union h a b in
  if find h a = find h b && RelSet.mem (a, b) ineqs then None
  else Some h'

  
let rec full env model h ineqs =
  match model with
  | [] -> Some h
  | propvar :: tail ->
      let propvar = begin match propvar with
      | Decision v | Unit v -> v
      end in
      begin match propvar with
      | Var i ->
          let op = begin try IntMap.find i env with
            Not_found -> raise (Error (UnboundPropVar i))
          end in
          let hopt = increment h op ineqs in
          begin match hopt with Some h' -> full env tail h' ineqs
          | None -> None
          end
      | Not i ->
          let Op (a, b) = begin try IntMap.find i env with
            Not_found -> raise (Error (UnboundPropVar i))
          end in
          full env tail h (RelSet.add (a, b) ineqs)
      end



module Solver = struct
  type t = Union_find.t * RelSet.t
  let empty n = Union_find.create n, RelSet.empty 

  let add_literal env var (h, ineqs) =
    let i, f = begin match var with
    | Var i -> i, (fun op -> increment h op ineqs, ineqs)
    | Not i -> i, (fun (Op (a, b)) -> Some h, RelSet.add (a, b) ineqs)
    end in
    let op = begin try IntMap.find i env with
      Not_found -> raise (Error (UnboundPropVar i))
    end in f op
        
    

end
        
      

  (* let rec step h eqs = *)
  (*   let a, b as e = RelSet.choose eqs in *)
  (*   let h' = union h a b in *)
  (*   if find h a = find h b && RelSet.mem e ineqs then false *)
  (*   else step h' (RelSet.remove e eqs) *)
  (* in try step h eqs with Not_found -> true *)

  



    








open Ast

type igraph =
  | Node of sat_var * igraph list
  | Leaf of sat_var
  | Src of igraph list

let rec string_of_igraph = function
  | Src nl -> String.concat ";\n" @@ List.map string_of_igraph nl
  | Node (v, nl) -> string_of_sat_var v ^ " -> " ^
                    String.concat "  ;" @@ List.map string_of_igraph nl
  | Leaf v -> string_of_sat_var v

let node_repr l gr =
  let rec search l f =
    match l with
    | [] -> raise Not_found
    | hd :: tl -> try f hd with Not_found -> search tl f
  in
  let rec step gr =
    match gr with
    | Node (v, _) | Leaf v when v = l -> gr
    | Node (_, nl) | Src nl -> search nl step
    | _ -> raise Not_found in
  try step gr
  with Not_found -> Leaf l


(* val add_implication : Clause.t -> sat_var -> igraph -> igraph *)
let add_implication cl l gr =
  (* Format.printf "Adding implication from %s to %s@." *)
  (*   (string_of_clause cl) (string_of_sat_var l); *)
  let rec add_edge o i gr =
    match gr with
    | Src nl ->
      let nl = List.filter (fun g -> g <> o) nl in
      Src (List.map (add_edge o i) nl)
    | Node (v, nl) when v = not_var i ->
      let nl = if List.mem o nl then nl else o :: nl in
      Node (v, nl)
    | Node (v, nl) -> Node (v, List.map (add_edge o i) nl)
    | Leaf v when v = not_var i -> Node (v, o :: [])
    | Leaf _ -> gr

  in
  let o = node_repr l gr in
  (* Format.printf "Before: %s@." @@ string_of_igraph gr; *)
  let gr = Clause.fold (add_edge o) cl gr in
  (* Format.printf "After: %s@." @@ string_of_igraph gr; *)
  gr


let empty pool =
  Clause.fold (fun l acc ->
      match acc with
      | Src nl -> Src (Leaf l :: nl)
      | _ -> acc) pool (Src [])


(* Naive cut *)
let cut gr =
  let nodes = match gr with
    | Src nl -> nl | _ -> assert false in
  List.fold_left (fun cl n ->
      match n with
      | Leaf _ -> cl
      | Node (v, _) -> Clause.add v cl
      | Src _ -> assert false) Clause.empty nodes

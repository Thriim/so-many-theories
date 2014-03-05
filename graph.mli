
open Ast

type igraph =
  | Node of sat_var * igraph list
  | Leaf of sat_var
  | Src of igraph list

val add_implication : Clause.t -> sat_var -> igraph -> igraph

val string_of_igraph : igraph -> string

val empty : Clause.t -> igraph

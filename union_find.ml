

(*
  Implementation inspired by
 - A persistent Union-Find Data Structure
       Conchon and Filliâtre
*)



module PArray = struct

  type 'a t = 'a data ref
  and 'a data =
  | Arr of 'a array
  | Diff of int * 'a * 'a t

  let init n f = ref (Arr (Array.init n f))

  let rec reroot t = match !t with
    | Arr _ -> ()
    | Diff (i, v, t') ->
      reroot t';
      begin match !t' with
      | Arr a as n ->
        let v' = a.(i) in
        a.(i) <- v;
        t := n;
        t' := Diff (i, v', t)
      | Diff _ -> assert false
      end

  let rec get t i = match !t with
    | Arr a -> a.(i)
    | Diff (j, v, t') ->
      reroot t';
        begin match !t' with
        | Arr a -> a.(i)
        | Diff _ -> assert false
        end

  let set t i v =
    reroot t;
    match !t with
    | Arr a as n ->
      let old = a.(i) in
      a.(i) <- v;
      let res = ref n in
      t := Diff (i, old, res);
      res
    | Diff _ -> assert false

end


type t = {
  rank : int PArray.t;
  mutable parent : int PArray.t
}

let create n = {
  rank = PArray.init n (fun _ -> 0);
  parent = PArray.init n (fun i -> i)

}

let rec find_aux parents i =
  let father = PArray.get parents i in
  if i == father then
    parents, i
  else
    let parents, result = find_aux parents father in
    let parents = PArray.set parents i result in
    parents , result


let find h x =
  let parents, cx = find_aux h.parent x in
  h.parent <- parents;
  cx

let union h x y =
  let repr_x = find h x in
  let repr_y = find h y in
  if repr_x != repr_y then begin
    let rank_x = PArray.get h.rank repr_x in
    let rank_y = PArray.get h.rank repr_y in
    if rank_x > rank_y then
      { h with parent = PArray.set h.parent repr_y repr_x }
    else if rank_x < rank_y then
      { h with parent = PArray.set h.parent repr_x repr_y }
    else
      { rank = PArray.set h.rank repr_x (rank_x + 1);
        parent = PArray.set h.parent repr_y repr_x }
  end
  else h

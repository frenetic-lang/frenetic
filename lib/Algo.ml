module Topological(V:Graph.Sig.ORDERED_TYPE) = struct
  open Core.Std

  module G = Graph.Persistent.Digraph.ConcreteBidirectional(struct
    include V
    let equal a b = compare a b = 0
    (* NB: This is ok since the underlying graph implementation is a persistent,
     * i.e., functional graph and does not make use of the hash function. Even
     * if this weren't the case, it'd still be correct, though extremely
     * ineffeicient.
     *)
    let hash _ = 0
  end)

  include Graph.Topological.Make(G)

  let pairs (ns : 'a list) : ('a * 'a) list =
    let rec loop l acc =
      match l with
        | []      -> acc
        | (x::xs) ->
          let acc' = List.fold xs ~init:acc ~f:(fun acc y ->
            match V.compare x y with
            | -1 -> (x, y)::acc
            |  1 -> (y, x)::acc
            |  0 -> acc
            |  _ -> assert false) in
          loop xs acc' in
    loop ns []

  let sort (ns : 'a list) : 'a list =
    (* Add all the vertices to the graph first. This is important in case there
     * is a vertex that is not involved in any edge. If this step wasn't take,
     * then that vertex would be dropped from the result. *)
    let g = List.fold ns ~init:G.empty ~f:(fun acc n -> G.add_vertex acc n) in
    let g = List.fold (pairs ns) ~init:g ~f:(fun acc (n, m) ->
      G.add_edge acc n m) in
    fold (fun n ns -> n::ns) g []
end

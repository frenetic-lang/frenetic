module type PARAM = sig

  type node
  type edge_label

end

module type DIRECTED_GRAPH = sig
 
  type node
  type edge_label
 
  type t

  val empty : unit -> t

  val add_edge : t -> node -> edge_label -> node -> unit

  val all_nodes : t -> node list

  val shortest_path : t -> node -> node -> node list

end 


let singleton_hashtbl k v =
  let h = Hashtbl.create 100 in
  Hashtbl.add h k v;
  h

module Make (Param : PARAM) : DIRECTED_GRAPH = struct

  type node = Param.node

  type edge_label = Param.edge_label

  (* Map from sources to a table of sinks and the edge label *)
  type t = { 
    graph : (node, (node, edge_label) Hashtbl.t) Hashtbl.t;
    nodes : (node, unit) Hashtbl.t
  }

  let empty () : t = 
    { graph = Hashtbl.create 100; nodes = Hashtbl.create 10 }

  let add_edge (g : t) (s : node) (x : edge_label) (d : node) =
    begin
      try 
        let s_edges_out = Hashtbl.find g.graph s in
        if Hashtbl.mem s_edges_out d then
          failwith "edge exists"
        else
            Hashtbl.add s_edges_out d x
      with
        | Not_found ->
          Hashtbl.add g.graph s (singleton_hashtbl d x)
    end;
    (if not (Hashtbl.mem g.nodes s) then
      Hashtbl.add g.nodes s ());
    (if not (Hashtbl.mem g.nodes d) then
      Hashtbl.add g.nodes d ())

  let all_nodes g =
    Hashtbl.fold (fun n _ lst -> n :: lst) g.nodes []

  let succs g s =
    try
      let succ_tbl = Hashtbl.find g.graph s in
      Hashtbl.fold (fun vx edge lst -> (vx :: lst))  succ_tbl []
    with
        Not_found -> failwith "node does not exist in succs"

  let shortest_path g s d = 
    let rec loop fringe = 
      match fringe with
        | [] -> []
        | [] :: _ -> failwith "impossible case during BFS"
        | (x :: xs) :: rest ->
          let nexts = succs g x in
          if List.mem s nexts then
            loop rest (* avoid silly cycles *)
          else if List.mem d nexts then
            (d :: x :: xs)
          else
            loop (fringe @ List.map (fun y -> y :: x :: xs) nexts) in
    if s = d then
      [s]
    else
      loop (List.map (fun x -> [x]) (succs g s))


end




module Params = struct

  type node = MininetTypes.node

  type edge_label = int (* egress port, bogus for hosts *)

end

module PolicyGraph = Make (Params)


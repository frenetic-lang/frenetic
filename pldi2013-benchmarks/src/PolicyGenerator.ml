open Printf

type +'a tree = Tree of 'a * 'a tree list

module type PARAM = sig

  type node
  type edge_label

  val string_of_node : node -> string
  val string_of_edge_label : edge_label -> string

  val node_compare : node -> node -> int
end

module type DIRECTED_GRAPH = sig
 
  type node
  type edge_label
 
  type t

  val empty : unit -> t

  val add_edge : t -> node -> edge_label -> int -> node -> unit

  val all_nodes : t -> node list

  val floyd_warshall : t -> (node * node list * node) list
  val prim : t -> t

  val path_with_edges : t -> node list -> (node * int * edge_label * node) list

  val to_string : t -> string

  val succs : t -> node -> (node * edge_label) list

end 


let singleton_hashtbl k v =
  let h = Hashtbl.create 100 in
  Hashtbl.add h k v;
  h

module Make (Param : PARAM) : DIRECTED_GRAPH
  with type node = Param.node
  and type edge_label = Param.edge_label = struct

  type node = Param.node

  type edge_label = Param.edge_label
  type weight = int

  (* Map from sources to a table of sinks and the edge label *)
  type t = { 
    graph : (node, (node, (int * edge_label)) Hashtbl.t) Hashtbl.t;
    nodes : (node, unit) Hashtbl.t
  }

  let to_string (g : t) : string = 
    let buf = Buffer.create 200 in
    Hashtbl.iter
      (fun src succs ->
        Hashtbl.iter 
          (fun dst (weight, edge) ->
            Buffer.add_string buf (Param.string_of_node src);
            Buffer.add_string buf " -- ";
            Buffer.add_string buf (string_of_int weight);
            Buffer.add_string buf " -- ";
            Buffer.add_string buf (Param.string_of_edge_label edge);
            Buffer.add_string buf " --> ";
            Buffer.add_string buf (Param.string_of_node dst);
            Buffer.add_string buf "\n")
          succs)
      g.graph;
    Buffer.contents buf

  let empty () : t = 
    { graph = Hashtbl.create 100; nodes = Hashtbl.create 10 }

  let add_node (g : t) (s : node) = 
    if not (Hashtbl.mem g.nodes s) then
      begin
        Hashtbl.add g.nodes s ();
        Hashtbl.add g.graph s (Hashtbl.create 100)
      end

  let add_edge (g : t) (s : node) (x : edge_label) (w : int) (d : node) =
    begin
      try 
        let s_edges_out = Hashtbl.find g.graph s in
        if Hashtbl.mem s_edges_out d then
          failwith "edge exists"
        else
            Hashtbl.add s_edges_out d (w, x)
      with
        | Not_found ->
          Hashtbl.add g.graph s (singleton_hashtbl d (w, x))
    end;
    (if not (Hashtbl.mem g.nodes s) then
      Hashtbl.add g.nodes s ());
    (if not (Hashtbl.mem g.nodes d) then
      Hashtbl.add g.nodes d ())

  let all_nodes g =
    Hashtbl.fold (fun n _ lst -> n :: lst) g.nodes []

  let undirected_edges g : (node * weight * edge_label * node) list = 
    Hashtbl.fold 
      (fun vx1 tbl lst ->
        Hashtbl.fold
          (fun vx2 (w, x) lst ->
            if Param.node_compare vx1 vx2 < 0 then
              (vx1, w, x, vx2) :: lst
            else
              lst)
          tbl lst)
      g.graph []

  let num_nodes (g : t) : int = Hashtbl.length g.nodes

  let succs g s =
    try
      let succ_tbl = Hashtbl.find g.graph s in
      Hashtbl.fold (fun vx (_, edge) lst -> ((vx, edge) :: lst))  succ_tbl []
    with
        Not_found -> 
          failwith (sprintf "node %s does not exist in the graph"
                      (Param.string_of_node s))

  let get_edge g s t = 
    try
      match Hashtbl.find (Hashtbl.find g.graph s) t with
        | (w, x) -> Some (w, x)
    with Not_found -> None


   (* assumes all edges have weight 1, kills singles *)
  let prim (g : t) : t =
    let tree = empty () in
    match all_nodes g with
      | [] -> tree
      | (vx :: _) -> (* gg  *)
        add_node g vx;
        let by_weight (_, w1, _, _) (_, w2, _, _) = compare w1 w2 in
        let edges = List.sort by_weight (undirected_edges g) in
         List.iter
           (fun (vx1, w, x, vx2) ->
             if not (Hashtbl.mem tree.nodes vx1 &&
                       Hashtbl.mem tree.nodes vx2) then
               begin
                 add_edge tree vx1 x w vx2;
                 match get_edge g vx2 vx1 with
                   | Some (_, x') -> add_edge tree vx2 x' w vx1
                   | None -> failwith "prim: undirected graph needed"
               end)
           edges;
        tree

  let neighbor_matrix (g : t) : (int * node list) array array =
    let n = num_nodes g in
    let vxs = Array.of_list (all_nodes g) in
    Array.init n
      (fun i ->
        Array.init n
          (fun j ->
            if i = j then
              (0, [vxs.(i)])
            else match get_edge g vxs.(i) vxs.(j) with
              | Some (w, _)  -> (w, [vxs.(i); vxs.(j)])
              | None -> (5000, []))) (* TODO(arjun): HACK *)

  let floyd_warshall (g : t) : (node * node list * node) list = 
    let matrix = neighbor_matrix g in
    let n = num_nodes g in
    let dist i j = match matrix.(i).(j) with
      | (d, _) -> d in
    let path i j = match matrix.(i).(j) with
      | (_, p) -> p in
    for k = 0 to n - 1 do
      for i = 0 to n - 1 do
        for j = 0 to n - 1 do
          if dist i k + dist k j < dist i j then
            matrix.(i).(j) <- (dist i k + dist k j, 
                               path i k @ List.tl (path k j))
        done
      done
    done;
    let paths = ref [] in
    let vxs = Array.of_list (all_nodes g) in
    Array.iteri
      (fun i array -> 
        Array.iteri 
          (fun j elt -> 
            let (_, p) = elt in
            paths := (vxs.(i), p, vxs.(j)) :: !paths)
          array;
        printf "\n";
      )
      matrix;
  !paths

  let rec path_with_edges (g : t) (lst : node list) = match lst with
    | [] -> []
    | [_] -> []
    | (x::y::rest) ->
      match get_edge g x y with
        | None -> failwith "not a path"
        | Some (w,l) -> (x,w,l,y) :: (path_with_edges g (y::rest))

end



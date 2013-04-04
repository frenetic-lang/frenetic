module H = Hashtbl
module Q = Queue
module M = OpenFlowTypes
module P = OpenFlowTypes

module SwSet = Set.Make(
  struct
    let compare = Pervasives.compare
    type t = M.switchId
  end)

module type GRAPH =
sig
  type a = M.switchId
  type b = P.portId
  type h = int
  type graph
  val create : unit -> graph
  val add_switch : graph -> a -> unit
  val add_edge : graph -> a -> b -> a -> b -> unit
  val add_host_edge : graph -> h -> a -> b -> unit
  val shortest_path : graph -> a -> a -> a list
  val get_ports : graph -> a -> a -> (b*b)
  (* val get_ports : graph -> a -> b list *)
  val nodes : graph -> SwSet.t
  (* val get_other_port : graph -> a -> b -> (a*b) option *)
  val next_hop : graph -> a -> b -> a
  val get_host_port : graph -> h -> (a*b) option
  val get_nbrs : graph -> a -> a list
  val has_node : graph -> a -> bool
  val del_edge : graph -> a -> b -> unit
  val del_edges : graph -> (a*b) list -> unit
  val copy : graph -> graph
  val to_string : graph -> string
  exception NoPath of string*string
  exception NotFound of string
end

module Graph : GRAPH =
  struct
    type a = M.switchId
    type b = P.portId
    type h = int
    type graph = ((a,(b, (a*b)) H.t) H.t) * ((h, (a*b)) H.t)
    exception NoPath of string*string
    exception NotFound of string
	
    let port_tbl_to_string portTbl =
      String.concat ";\n\t\t" (H.fold (fun port (sw',port') acc -> (Printf.sprintf "%ld -> (%Ld,%ld)" port sw' port') :: acc) portTbl [])

    let to_string (graph,_) =
      String.concat ";\n\t" (H.fold (fun sw portTbl acc -> (Printf.sprintf "%Ld -> {%s}" sw (port_tbl_to_string portTbl)) :: acc) graph [])
    let add_switch ((graph, _) : graph) (sw : a) = H.add graph sw (H.create 5)
    let add_edge (graph, _) sw1 pt1 sw2 pt2 = 
      let swTbl = (try (H.find graph sw1) with 
	  Not_found -> let foo = H.create 5 in
		       H.add graph sw1 foo;
		       foo) in
      H.add swTbl pt1 (sw2, pt2) 
    let add_host_edge (_, hgraph) h sw pt = H.add hgraph h (sw,pt)
    let del_edge (graph, _) sw1 pt1 = try H.remove (H.find graph sw1) pt1 with _ -> raise (NotFound(Printf.sprintf "Can't find %Ld to del_edge %ld\n" sw1 pt1))
    let del_edges graph edges = List.iter (fun (a,b) -> del_edge graph a b) edges

    let create () = (H.create 5, H.create 5)

    let get_nbrs' graph sw = 
      try (H.fold (fun pt1 (sw2, pt2) acc -> sw2 :: acc) (H.find graph sw) []) 
      with _ -> []

    let get_nbrs (graph, _) sw = get_nbrs' graph sw

    let copy (graph1, graph2) = let newgraph1 = H.create (H.length graph1) in
				let () = H.iter (fun k v -> H.add newgraph1 k (H.copy v)) graph1 in
				(newgraph1, H.copy graph2)

    let copy' graph = H.copy graph

    let rec bfs' target graph queue =
      let (sw, path) = (Q.take queue) in
      match sw = target with
	| true -> path
	| false -> let () = List.iter (fun x -> Q.add (x, x :: path) queue) (get_nbrs' graph sw); 
		     H.remove graph sw in
		   bfs' target graph queue

    let bfs graph src dst = 
      let q = Queue.create () in
      let () = Q.add (src, [src]) q in
      try (bfs' dst (copy' graph) q) 
      with Queue.Empty -> raise (NoPath(Int64.to_string src, Int64.to_string dst))

    let shortest_path topo src dst = 
      let graph,_ = topo in
      Printf.printf "shortest_path %Ld %Ld\n" src dst;
      try List.rev (bfs graph src dst) with 
	| NoPath(s1,s2) -> Printf.printf "Couldn't find path in graph %s\n" (to_string topo);
	  raise (NoPath(s1,s2))

    let get_ports (topo,_) s1 s2 = 
      let () = Printf.printf "get_ports %Ld %Ld\n" s1 s2 in
      let s1Tbl = try (H.find topo s1) with Not_found -> raise (NotFound(Printf.sprintf "Can't find switch %Ld to get_ports to %Ld\n" s1 s2)) in
      let unwrap (Some foo) = foo in
      try unwrap (H.fold (fun pt (sw, pt') acc -> if sw = s2 then Some (pt, pt') else acc) s1Tbl None) 
      with _ -> raise (NotFound(Printf.sprintf "Can't find ports to switch %Ld from %Ld\n" s2 s1))

    let next_hop (topo,_) sw p = 
      let swTbl = try (Hashtbl.find topo sw) 
	with Not_found -> raise (NotFound(Printf.sprintf "Can't find %Ld to get next_hop\n" sw)) in
      try fst (Hashtbl.find swTbl p) 
      with Not_found -> raise (NotFound(Printf.sprintf "Can't find port %ld to get next_hop\n" p))

    let nodes (topo,_) = H.fold (fun sw sw' acc -> SwSet.add sw acc) topo SwSet.empty
    let get_host_port (_,topo) host = 
      try Some (H.find topo host) with _ -> None
    let has_node (graph,_) = H.mem graph
  end

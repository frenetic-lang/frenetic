module H = Hashtbl
module Q = Queue
module M = OpenFlow0x01
module P = Packet

module type GRAPH =
sig
  type node = 
    | Switch of OpenFlow0x01.switchId
    | Host of Packet.dlAddr
  type n = node
  type p = Packet.portId
  type graph
  val create : unit -> graph
  val add_node : graph -> n -> unit
  val add_switch : graph -> Int64.t -> unit
  val add_host : graph -> int -> unit
  val add_edge : graph -> n -> p -> n -> p -> unit
  val add_host_edge : graph -> n -> n -> p -> unit
  val shortest_path : graph -> n -> n -> n list
  val get_ports : graph -> n -> n -> (b*b)
  val get_switches : graph -> n list
  val get_hosts : graph -> n list
  val get_nodes : graph -> n list
  (* val get_ports : graph -> n -> p list *)
  (* val get_other_port : graph -> n -> p -> (a*b) option *)
  val next_hop : graph -> n -> p -> a
  val get_nbrs : graph -> n -> n list
  val has_node : graph -> n -> bool
  val del_edge : graph -> n -> p -> unit
  val del_edges : graph -> (a*b) list -> unit
  val del_link : graph -> n -> n -> unit
  val del_links : graph -> (a*a) list -> unit
  val del_node : graph -> n -> unit
  val copy : graph -> graph
  (* val to_string : graph -> string *)
  (* val node_to_string : node -> string *)
  exception NoPath of node*node
  exception NotFound of node
end

module Graph : GRAPH =
  struct
    type node = 
      | Switch of OpenFlow0x01.switchId
      | Host of Packet.dlAddr
    type n = node
    type p = P.portId
    type h = int
    (* sw -> port -> (sw*port) *)
    type portTbl_t = ((n,(p, (n*p)) H.t) H.t)
    type graph = portTbl_t
    exception NoPath of node*node
    exception NotFound of node

    (* let node_to_string nd = match nd with *)
    (*   | Switch sw -> Printf.sprintf "Switch %Ld" sw *)
    (*   | Host h -> Printf.sprintf "Host %d" h *)

    let get_switches graph = H.fold (fun k _ acc -> match k with
      | Switch _ -> k :: acc
      | Host _ -> acc) graph []

    let get_hosts graph = H.fold (fun k _ acc -> match k with
      | Host _ -> k :: acc
      | Switch _ -> acc) graph []

    let get_nodes graph = H.fold (fun k _ acc -> k :: acc) graph []

    (* let port_tbl_to_string portTbl = *)
    (*   String.concat ";\n\t\t" (H.fold (fun port (sw',port') acc -> (Printf.sprintf "%ld -> (%s,%ld)"  *)
    (* 								      port  *)
    (* 								      (node_to_string sw')  *)
    (* 								      port') :: acc)  *)
    (* 				 portTbl []) *)

    (* let to_string graph = *)
    (*   String.concat ";\n\t" (H.fold (fun sw portTbl acc -> (Printf.sprintf "%s -> {%s}"  *)
    (* 							      (node_to_string sw)  *)
    (* 							      (port_tbl_to_string portTbl)) :: acc)  *)
    (* 			       graph []) *)
    let add_node graph (sw : a) = H.add graph sw (H.create 5)
    let add_switch graph sw = add_node graph (Switch sw)
    let add_host graph h = add_node graph (Host h)
    let add_edge graph sw1 pt1 sw2 pt2 = 
      let swTbl = (try (H.find graph sw1) with 
	  Not_found -> let foo = H.create 5 in
		       H.add graph sw1 foo;
		       foo) in
      H.add swTbl pt1 (sw2, pt2) 

    let add_host_edge graph h sw pt = 
      add_edge graph h (Int32.of_int 0) sw pt; 
      add_edge graph sw pt h (Int32.of_int 0)

    let del_edge graph sw1 pt1 = 
      try H.remove (H.find graph sw1) pt1 with 
	  _ -> raise (NotFound sw1)

    let del_edges graph edges = List.iter (fun (a,b) -> del_edge graph a b) edges

    let create () = H.create 5

    let get_nbrs graph sw = 
      try (H.fold (fun pt1 (sw2, pt2) acc -> sw2 :: acc) (H.find graph sw) []) 
      with _ -> []

    let get_nbr_links graph sw = 
      try (H.fold (fun pt1 (sw2, pt2) acc -> (sw2, pt2) :: acc) (H.find graph sw) []) 
      with _ -> []

    let copy graph = let newgraph = H.create (H.length graph) in
		     H.iter (fun k v -> H.add newgraph k (H.copy v)) graph;
		     newgraph

    let rec bfs' target graph queue =
      let (sw, path) = (Q.take queue) in
      match sw = target with
	| true -> path
	| false -> 
	  List.iter (fun x -> Q.add (x, x :: path) queue) (get_nbrs graph sw); 
	  H.remove graph sw;
	  bfs' target graph queue

    let bfs graph src dst = 
      let q = Queue.create () in
      let () = Q.add (src, [src]) q in
      try (bfs' dst (copy graph) q) 
      with Queue.Empty -> raise (NoPath(src, dst))

    let shortest_path graph src dst = 
      try List.rev (bfs graph src dst) with 
	| NoPath(s1,s2) -> Printf.printf "Couldn't find path in graph %s\n" (to_string graph);
	  raise (NoPath(s1,s2))

    let get_ports topo s1 s2 = 
      let s1Tbl = try (H.find topo s1) with 
	  Not_found -> raise (NotFound( s1, s2)) in
      let unwrap (Some foo) = foo in
      try unwrap (H.fold (fun pt (sw, pt') acc -> if sw = s2 then Some (pt, pt') else acc) s1Tbl None) 
      with _ -> raise (NotFound(s2))

    let del_link graph sw1 sw2 = 
      let p1,p2 = get_ports graph sw1 sw2 in
      del_edge graph sw1 p1
    let del_links graph edges = List.iter (fun (a,b) -> del_link graph a b) edges

    let del_node graph sw = 
      let nbrs = get_nbrs graph sw in
      try List.iter (fun a -> del_link graph a sw; del_link graph sw a) nbrs with _ -> ();
	H.remove graph sw


    let next_hop topo sw p = 
      let swTbl = try (Hashtbl.find topo sw) 
	with Not_found -> raise (NotFound sw) in
      try fst (Hashtbl.find swTbl p) 
      with Not_found -> raise (NotFound( p))

    let has_node graph = H.mem graph
  end

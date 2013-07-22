module H = Hashtbl
module Q = Queue
module M = NetCore_Types

module type GRAPH =
sig
  type node = 
      Host of int
    | Switch of M.switchId
  type a = node
  type b = M.portId
  type graph
  val create : unit -> graph
  val add_node : graph -> a -> unit
  val add_switch : graph -> Int64.t -> unit
  val add_host : graph -> int -> unit
  val add_port : graph -> a -> b -> unit
  val add_edge : graph -> a -> b -> a -> b -> unit
  val add_host_edge : graph -> a -> a -> b -> unit
  val shortest_path : graph -> a -> a -> a list
  val get_ports : graph -> a -> a -> (b*b)
  val ports_of_switch : graph -> a -> b list
  val edge_ports_of_switch : graph -> NetCore_Types.switchId -> b list
  val get_switches : graph -> NetCore_Types.switchId list
  val get_hosts : graph -> a list
  val get_nodes : graph -> a list
  val get_switches_and_ports : graph -> (NetCore_Types.switchId * b list) list
  (* val get_ports : graph -> a -> b list *)
  (* val get_other_port : graph -> a -> b -> (a*b) option *)
  val next_hop : graph -> a -> b -> a
  val get_nbrs : graph -> a -> a list
  val has_node : graph -> a -> bool
  val del_edge : graph -> a -> b -> unit
  val del_edges : graph -> (a*b) list -> unit
  val del_link : graph -> a -> a -> unit
  val del_links : graph -> (a*a) list -> unit
  val del_node : graph -> a -> unit
  val copy : graph -> graph
  val to_string : graph -> string
  val node_to_string : node -> string
  exception NoPath of string*string
  exception NotFound of string
end

module Graph : GRAPH =
  struct
    type node = 
        Host of int
      | Switch of M.switchId
    type a = node
    type b = M.portId
    type h = int
    (* sw -> port -> (sw*port) *)
    type portTbl_t = ((a,(b, (a*b) option) H.t) H.t)
    (* h -> (sw*port) *)
    type graph = portTbl_t
    exception NoPath of string*string
    exception NotFound of string

    let node_to_string nd = match nd with
      | Switch sw -> Printf.sprintf "Switch %Ld" sw
      | Host h -> Printf.sprintf "Host %d" h

    let get_switches graph = H.fold (fun k _ acc -> match k with
      | Switch sw -> sw :: acc
      | Host _ -> acc) graph []

    let get_hosts graph = H.fold (fun k _ acc -> match k with
      | Host _ -> k :: acc
      | Switch _ -> acc) graph []

    let get_nodes graph = H.fold (fun k _ acc -> k :: acc) graph []

    let port_tbl_to_string portTbl =
      String.concat ";\n\t\t" (H.fold (fun port sw_p' acc -> match sw_p' with
					  | Some (sw',port') -> (Printf.sprintf "%ld -> (%s,%ld)" 
                                                                      port 
                                                                      (node_to_string sw') 
                                                                      port') :: acc
					  | None -> (Printf.sprintf "%ld"
                                                                      port :: acc) ) 
                                 portTbl [])

    let to_string graph =
      String.concat ";\n\t" (H.fold (fun sw portTbl acc -> (Printf.sprintf "%s -> {%s}" 
                                                              (node_to_string sw) 
                                                              (port_tbl_to_string portTbl)) :: acc) 
                               graph [])
    let add_node graph (sw : a) = H.add graph sw (H.create 5)
    let add_switch graph sw = add_node graph (Switch sw)
    let add_host graph h = add_node graph (Host h)
    let add_edge graph sw1 pt1 sw2 pt2 = 
      let swTbl = (try (H.find graph sw1) with 
          Not_found -> let foo = H.create 5 in
                       H.add graph sw1 foo;
                       foo) in
      H.add swTbl pt1 (Some (sw2, pt2))

    let add_port graph sw pt =
      let swTbl = (try (H.find graph sw) with 
          Not_found -> let foo = H.create 5 in
                       H.add graph sw foo;
                       foo) in
      H.add swTbl pt None

    let default_host_port = Int32.of_int 0
    let add_host_edge graph h sw pt = 
      add_edge graph h default_host_port sw pt; 
      add_edge graph sw pt h default_host_port

    let del_edge graph sw1 pt1 = 
      try H.remove (H.find graph sw1) pt1 with 
          _ -> raise (NotFound(Printf.sprintf "Can't find %s to del_edge %ld\n" (node_to_string sw1) pt1))

    let del_edges graph edges = List.iter (fun (a,b) -> del_edge graph a b) edges

    let create () = H.create 5

    let get_nbrs graph sw = 
      try (H.fold (fun pt1 sw_pt acc -> match sw_pt with Some (sw2, pt2) -> sw2 :: acc | None -> acc) (H.find graph sw) []) 
      with _ -> []

    let get_nbr_links graph sw = 
      try (H.fold (fun pt1 sw_pt acc -> match sw_pt with Some (sw2, pt2) -> (sw2, pt2) :: acc | None -> acc) (H.find graph sw) []) 
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
      with Queue.Empty -> raise (NoPath(node_to_string src, node_to_string dst))

    let shortest_path graph src dst = 
      (* Printf.printf "shortest_path %s %s\n" (node_to_string src) (node_to_string dst); *)
      try List.rev (bfs graph src dst) with 
        | NoPath(s1,s2) -> Printf.printf "Couldn't find path in graph %s\n" (to_string graph);
          raise (NoPath(s1,s2))

    let get_ports topo s1 s2 = 
      (* Printf.printf "get_ports %s %s\n" (node_to_string s1) (node_to_string s2); *)
      let s1Tbl = try (H.find topo s1) with 
          Not_found -> raise (NotFound(Printf.sprintf "Can't find %s to get_ports to %s\n" 
                                         (node_to_string s1) (node_to_string s2))) in     
      let unwrap (Some foo) = foo in
      try unwrap (H.fold (fun pt sw_pt acc -> match sw_pt with
	| Some (sw, pt') -> if sw = s2 then Some (pt, pt') else acc
	| None -> acc) s1Tbl None) 
      with _ -> raise (NotFound(Printf.sprintf "Can't find ports to %s from %s\n" 
                                  (node_to_string s2) (node_to_string s1)))

    let ports_of_switch topo s = 
      let sTbl = try (H.find topo s) with 
          Not_found -> raise (NotFound(Printf.sprintf "Can't find %s to get ports_of_switch\n" 
                                         (node_to_string s))) in
      H.fold (fun pt _ acc -> pt :: acc) sTbl []

    let edge_ports_of_switch topo s = 
      let sTbl = try (H.find topo (Switch s)) with 
          Not_found -> raise (NotFound(Printf.sprintf "Can't find %s to get ports_of_switch\n" 
                                         (node_to_string (Switch s)))) in
      H.fold (fun pt sw_pt acc -> match sw_pt with
	| None -> pt :: acc
	| Some (Switch _,_) -> acc
	| Some (Host _, _) -> pt :: acc) sTbl []

    let get_switches_and_ports graph = H.fold (fun k pTbl acc -> match k with
      | Switch sw -> (sw, ports_of_switch graph (Switch sw)) :: acc
      | _ -> acc) graph []

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
        with Not_found -> raise (NotFound(Printf.sprintf "Can't find %s to get next_hop\n" (node_to_string sw))) in
      try match (Hashtbl.find swTbl p) with
	| Some (sw', p') -> sw'
	| None -> raise (NotFound(Printf.sprintf "next_hop: Port %ld is not connected\n" p))
      with Not_found -> raise (NotFound(Printf.sprintf "Can't find port %ld to get next_hop\n" p))

    let has_node graph = H.mem graph
  end

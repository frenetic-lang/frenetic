open Util
open Types
open Graph


module EdgeOrd = struct
  type t = Node.t * Link.t * Node.t
  let src (s,_,_) = s
  let dst (_,_,d) = d
  let label (_,l,_) = l
  let compare = Pervasives.compare
end

module Weight = struct
  open Link
  type t = Int64.t
  type label = Link.t
  let weight l = l.cost
  let compare = Int64.compare
  let add = Int64.add
  let zero = Int64.zero
end

module EdgeSet = Set.Make(EdgeOrd)

module EdgeMap = Map.Make(EdgeOrd)

module G = Persistent.Digraph.ConcreteBidirectionalLabeled(Node)(Link)
module V = G.V
module E = G.E
module NH = Node.NodeHash
module Dij = Path.Dijkstra(G)(Weight)

exception NotFound of string
exception NoPath of string * string

type t = G.t * Node.attr_tbl

(* Alias for add_vertex *)
let add_node ((g,t):t) (n:Node.t) : t =
  (G.add_vertex g n,t)

(* Add a host, given its name, MAC address and IP, to the graph *)
let add_host ((g,t):t) (h:string) (m:Packet.dlAddr) (p:Packet.nwAddr) (i:int)
    : t =
  let attr = { node_type = Host; dev_id = 0L; name = h ; ip = p; mac = m} in
  let node = Node.create i in
  NH.replace t node attr;
  let g' = G.add_vertex g node in
  (g',t)


(* Add a switch (from it's name and id) to the graph *)
let add_switch ((g,t):t) (s:switchId) (i:int) : t =
  let attr = {default with node_type = Switch ; dev_id = s} in
  let node = Node.create i in
  NH.replace t node attr;
  let g' = G.add_vertex g node in
  (g',t)

(* Add an edge between particular ports on two switches *)
let add_switch_edge ((g,t):t) (s:Node.t) (sp:portId) (d:Node.t) (dp:portId) : t =
  let l = {Link.default with Link.srcport = sp; Link.dstport = dp} in
  (G.add_edge_e g (s,l,d),t)

(****** Accessors ******)
(* Get a list of all the vertices in the graph *)
let get_vertices ((g,_):t) : (V.t list) =
  G.fold_vertex (fun v acc -> v::acc) g []

  (* Get a list of all the edges in the graph. *)
let get_edges ((g,_):t) : (E.t list) =
  G.fold_edges_e (fun e acc -> e::acc) g []

(* For a given pair of nodes in the graph, return the list of port pairs that
   connect them. Raise NotFound if there are the two nodes are not connected *)
let get_ports ((g,t):t) (s:V.t) (d:V.t) : (portId * portId) =
  let es = G.find_all_edges g s d in
  if List.length es = 0
  then raise (NotFound (Printf.sprintf "Can't find %s to get_ports to %s\n"
                          (Node.to_string s t) (Node.to_string d t)))
  else let e = List.hd es in
       (Link.srcport e, Link.dstport e)

(* Get a list of the hosts out in the graph. Returns an empty list if
   there are no hosts.  *)
let get_hosts ((g,t):t) : (Node.t list) =
  G.fold_vertex (fun v acc ->
    let attr = NH.find t v in
    match attr.node_type with
    | Host -> v::acc
    | _ -> acc
  ) g []


  (* Get a list of the switches out in the graph. Returns an empty list if
     there are no switches.  *)
let get_switches ((g,t):t) : (Node.t list) =
  G.fold_vertex (fun v acc ->
    let attr = NH.find t v in
    match attr.node_type with
    | Switch -> v::acc
    | _ -> acc
  ) g []


  (* Get a list of the switch IDs in the graph. Returns an empty list if
     there are no switches.  *)
let get_switchids ((g,t):t) : (switchId list) =
  G.fold_vertex (fun v acc ->
    let attr = NH.find t v in
    match attr.node_type with
    | Switch -> (attr.dev_id)::acc
    | _ -> acc
  ) g []

(* Compute a topology with unit cost *)
let unit_cost ((g0,t):t) : t =
  let f (n1,l,n2) g : G.t =
    G.add_edge_e g (n1, { l with Link.cost = 1L }, n2) in
  let g = G.fold_vertex (fun v g -> G.add_vertex g v) g0 G.empty in
  let g = G.fold_edges_e (fun e g -> f e g) g0 g in
  (g,t)

  (* For a given node, return all its connected ports.
     Raise NotFound if the node is not in the graph *)
let ports_of_switch ((g,t):t) (s:Node.t) : portId list =
  let ss = try (G.succ_e g s)
    with Not_found -> raise (NotFound(Printf.sprintf
                                        "Can't find %s to get ports_of_switch\n"
                                        (Node.to_string s t))) in
  let sports = List.map
    (fun l -> Link.srcport l) ss in
  let ps = G.pred_e g s in
  let pports = List.map
    (fun l -> Link.dstport l) ps in
  sports @ pports

let next_hop ((g,t):t) (n:Node.t) (p:portId) : Node.t =
  let ss = try (G.succ_e g n)
    with Not_found -> raise (NotFound(Printf.sprintf
                                        "Can't find %s to get next_hop\n"
                                        (Node.to_string n t))) in
  let (_,_,d) = try (List.hd
                       (List.filter (fun e -> (Link.srcport e) = p) ss))
    with Failure hd -> raise (NotFound(
      Printf.sprintf "next_hop: Port %s on %s is not connected\n"
        (Int64.to_string p) (Node.to_string n t)))
  in d

(* Find the shortest path between two nodes using Dijkstra's algorithm,
   returning the list of edges making up the path. The implementation is from
   the ocamlgraph library.
   Raise NoPath if there is no such path. *)

let shortest_path ((g,_):t) (src:Node.t) (dst:Node.t) : E.t list =
  let ret,_ = Dij.shortest_path g src dst in ret

(* let shortest_path_v (g:t) (src:Node.t) (dst:Node.t) : V.t list = *)
(*   let visited = Hashtbl.create (nb_vertex g) in *)
(*   let previous =  Hashtbl.create (nb_vertex g) in *)
(*   let queue = Core.Std.Heap.create (fun (v,d) (v,d') -> compare d d') () in *)
(*   Core.Std.Heap.add queue (src,0); *)

(*   let rec mk_path current = *)
(*     if current = src then [src] else *)
(*       let prev = Hashtbl.find previous current in *)
(*       current::(mk_path prev) in *)

(*   let rec loop (current,distance) = *)
(*     if current = dst then () *)
(*     else begin *)
(*       iter_succ (fun next -> *)
(*         if Hashtbl.mem visited next then () *)
(*         else *)
(*           let next_dist = distance + 1 in *)
(*           Hashtbl.replace previous next current; *)
(*           Core.Std.Heap.add queue (next,next_dist) *)
(*       ) g current; *)
(*       Hashtbl.replace visited current 0; *)
(*       loop (Core.Std.Heap.pop_exn queue) end in *)
(*   loop (Core.Std.Heap.pop_exn queue); *)
(*   mk_path dst *)


  (*  let shortest_path (g:t) (src:Node.t) (dst:Node.t) : E.t list =
      let p,_ = Dij.shortest_path g src dst in
      if p = [] then raise (NoPath(Node.to_string src, Node.to_string dst))
      else p *)

let stitch (path:E.t list) : (portId option * Node.t * portId option) list =
  let hops = List.fold_left (fun acc e -> match acc with
    | [] ->
      [ (Some(Link.dstport e), Link.dst e , None)
      ; (None, Link.src e, Some(Link.srcport e))]
    | (inp, srcnode, outp)::tl ->
      let srcport = Some (Link.srcport e) in
      let dstport = Some (Link.dstport e) in
      let dstnode = Link.dst e in
      let src = (inp, srcnode, srcport) in
      let dst = (dstport, dstnode, None) in
      dst::src::tl
  ) [] path in
  List.rev hops

let floyd_warshall ((g,t):t) : ((V.t * V.t) * V.t list) list =
  let add_opt o1 o2 =
    match o1, o2 with
      | Some n1, Some n2 -> Some (Int64.add n1 n2)
      | _ -> None in
  let lt_opt o1 o2 =
    match o1, o2 with
      | Some n1, Some n2 -> n1 < n2
      | Some _, None -> true
      | None, Some _ -> false
      | None, None -> false in
  let make_matrix (g:G.t) =
    let n = G.nb_vertex g in
    let nodes = Array.of_list (get_vertices (g,t)) in
    Array.init n
      (fun i -> Array.init n
        (fun j -> if i = j then (Some 0L, [nodes.(i)])
          else
            try
              let l = G.find_edge g nodes.(i) nodes.(j) in
              (Some (Link.cost l), [nodes.(i); nodes.(j)])
            with Not_found ->
              (None,[]))) in
  let matrix = make_matrix g in
  let n = G.nb_vertex g in
  let dist i j = fst (matrix.(i).(j)) in
  let path i j = snd (matrix.(i).(j)) in
  for k = 0 to n - 1 do
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        let dist_ikj = add_opt (dist i k) (dist k j) in
        if lt_opt dist_ikj (dist i j) then
          matrix.(i).(j) <- (dist_ikj, path i k @ List.tl (path k j))
      done
    done
  done;
  let paths = ref [] in
  let vxs = Array.of_list (get_vertices (g,t)) in
  Array.iteri (fun i array ->
    Array.iteri (fun j elt ->
      let (_, p) = elt in
      paths := ((vxs.(i),vxs.(j)),p) :: !paths) array;) matrix;
  !paths

let undirected_edges ((g,_):t): E.t list =
  G.fold_edges_e (fun e acc ->
    let v1 = E.src e and v2 = E.dst e in
    if V.compare v1 v2 < 0 then e::acc else acc) g []

let spanningtree ((g,t):t): G.t =
  let tree = ref G.empty in
  let vertices = get_vertices (g,t) in
  match vertices with
      [] -> !tree
    | (vx::_) ->
      tree := G.add_vertex (!tree) vx;
      let visited_nodes = Hashtbl.create 16 in
      Hashtbl.add visited_nodes vx 1;
      let by_weight e1 e2 = Int64.compare (Link.cost e1) (Link.cost e2) in
      let edges = List.sort by_weight (undirected_edges (g,t)) in
      let rec find_edge tbl l tree = match l with
          [] -> failwith "It cannot happen"
        | h::t -> let vx1 = E.src h and vx2 = E.dst h in
                  if Hashtbl.mem tbl vx1 && not (Hashtbl.mem tbl vx2) then
                    (Hashtbl.add tbl vx2 1;
                     tree := G.add_edge_e (!tree) h;
                     tree := G.add_edge_e (!tree) (Link.reverse h))
                  else if Hashtbl.mem tbl vx2 && not (Hashtbl.mem tbl vx1) then
                    (Hashtbl.add tbl vx1 1;
                     tree :=  G.add_edge_e (!tree) h;
                     tree := ( G.add_edge_e (!tree) (Link.reverse h)))
                  else find_edge tbl t tree in
      while Hashtbl.length visited_nodes != List.length vertices do
        find_edge visited_nodes edges tree
      done;
      !tree

  (* Produce a dot representation of the topology, usable by Graphviz *)
let to_dot ((g,t):t) =
  let edges = get_edges (g,t) in
  let es = list_intercalate (fun l -> Link.to_dot l t) "\n" edges in
  Printf.sprintf "digraph G {\n%s\n}" es

let to_string = to_dot


  (* Produce a Mininet script that implements the given topology *)
let to_mininet ((g,tbl):t) : string =
    (* Load static strings (maybe there's a better way to do this?) *)
  let prologue = load_file "static/mn_prologue.txt" in
  let epilogue = load_file "static/mn_epilogue.txt" in

    (* Check if an edge or its reverse has been added already *)
  let seen = ref EdgeSet.empty in
  let not_printable e =
    E.src e = E.dst e ||
    EdgeSet.mem e !seen ||
    EdgeSet.mem (Link.reverse e) !seen
  in

    (* Add the hosts and switches *)
  let add_hosts = G.fold_vertex
    (fun v acc ->
      let attr = NH.find tbl v in
      let add = match attr.node_type with
        | Host ->
          Printf.sprintf "    %s = net.addHost(\'%s\', mac=\'%s\', ip=\'%s\')\n"
            attr.name attr.name
            (Packet.string_of_mac attr.mac) (Packet.string_of_ip attr.ip)
        | _ ->
          Printf.sprintf
            "    s%Ld = net.addSwitch(\'s%Ld\')\n" attr.dev_id attr.dev_id in
      acc ^ add
    )
    g "" in

    (* Add links between them *)
  let links = G.fold_edges_e
    (fun e acc ->
      let add =
        if (not_printable e) then ""  (* Mininet links are bidirectional *)
        else
          let src = Str.global_replace (Str.regexp "[ ,]") ""
            (Node.to_string (E.src e) tbl) in
          let dst = Str.global_replace (Str.regexp "[ ,]") ""
            (Node.to_string (E.dst e) tbl) in
          Printf.sprintf "    net.addLink(%s, %s, %s, %s)\n"
            src dst
            (Int64.to_string (Link.srcport e))
            (Int64.to_string (Link.dstport e))
      in
      seen := EdgeSet.add e !seen;
      acc ^ add
    )
    g "" in
  prologue ^ add_hosts ^ links ^ epilogue


(* let from_dotfile_tbl = Parsers.from_dotfile_tbl *)
(* let from_dotfile = Parsers.from_dotfile *)
(* let from_gmlfile = Parsers.from_gmlfile *)

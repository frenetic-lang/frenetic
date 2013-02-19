module H = Hashtbl
module Q = Queue
module M = MessagesDef
module P = Packet

module SwSet = Set.Make(
  struct
    let compare = Pervasives.compare
    type t = M.switchId
  end)

module type GRAPH =
sig
  type a = M.switchId
  type b = P.portId
  type graph
  val create : unit -> graph
  val add_node : graph -> a -> unit
  val add_edge : graph -> a -> b -> a -> b -> unit
  val shortest_path : graph -> a -> a -> a list
  val get_port : graph -> a -> a -> b option
  (* val get_ports : graph -> a -> b list *)
  val nodes : graph -> a list
  (* val get_other_port : graph -> a -> b -> (a*b) option *)
  val get_host_port : graph -> a -> b option
  val get_nbrs : graph -> a -> a list
  val has_node : graph -> a -> bool
  exception NoPath of string*string
end

module Graph : GRAPH =
  struct
    type a = M.switchId
    type b = P.portId
    type graph = (a,(b, (a*b)) H.t) H.t
	

    exception NoPath of string*string

(* topo = (G, ports) *)
(* G = { sw -> {pt} -> (sw, pt) } *)

    let add_node (graph : graph) (sw : a) = H.add graph sw (H.create 5)
    let add_edge graph sw1 pt1 sw2 pt2 = H.add (H.find graph sw1) pt1 (sw2, pt2)
    let create () = H.create 5
    let get_nbrs graph sw = try (H.fold (fun pt1 (sw2, pt2) acc -> sw2 :: acc) (H.find graph sw) []) with _ -> []
    let copy graph = H.copy graph

    let rec bfs' target graph queue =
      let (sw, path) = (Q.take queue) in
      match sw = target with
	| true -> path
	| false -> let () = List.iter (fun x -> Q.add (x, x :: path) queue) (get_nbrs graph sw); H.remove graph sw in
		   bfs' target graph queue

    let bfs graph src dst = 
      let q = Queue.create () in
      let () = Q.add (src, [src]) q in
      try (bfs' dst (copy graph) q) 
      with Queue.Empty -> raise (NoPath(Int64.to_string src, Int64.to_string dst))

    let shortest_path = bfs

    let get_port topo s1 s2 = let () = Printf.printf "get_hop %Ld %Ld\n" s1 s2 in
			      try (H.fold (fun pt (sw, pt') acc -> if sw = s2 then (Some pt) else acc) (H.find topo s1) None) with _ -> None


(* let get_other_port topo sw p = try (Some (Hashtbl.find (Hashtbl.find (snd topo) s1) s2) *)
    let nodes topo = H.fold (fun sw sw' acc -> sw :: acc) topo []

    let get_host_port topo host = 
      try (H.fold (fun pt (sw, pt') acc -> Some pt') (Hashtbl.find topo host) None) with _ -> None
      
    let has_node  = H.mem
  end

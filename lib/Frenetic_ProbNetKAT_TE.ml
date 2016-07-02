open Core.Std
open Frenetic_Network
open Frenetic_ProbNetKAT_Interpreter
open Frenetic_ProbNetKAT_Routing
open Net
open Num
open Topology
open Routing_Types

include Interp(Hist)(PreciseProb)
include Routing(Prob)
open Pol

(********************************** Topology **********************************)
(* Check if a node is a host *)
let is_host (topo : Topology.t) (node : vertex) =
  Node.device (vertex_to_label topo node) = Node.Host

let is_switch (topo : Topology.t) (node : vertex) =
  Node.device (vertex_to_label topo node) = Node.Switch

let gen_node_pnk_id_map (topo : Topology.t) =
  let host_offset =
    Int.of_float (10. ** (Pervasives.ceil (log10 (Float.of_int (num_vertexes topo))) +. 1.)) in
  VertexSet.fold (Topology.vertexes topo)
    ~init:(VertexMap.empty, IntMap.empty)
    ~f:(fun (v_id, id_v) v ->
      let node_name = Node.name (vertex_to_label topo v) in
      let regexp = Str.regexp "\\([A-Za-z]+\\)\\([0-9]+\\)" in
      let _ = Str.string_match regexp node_name 0 in
      let offset = if is_host topo v then host_offset else 0 in
      let node_id = offset + int_of_string (Str.matched_group 2 node_name) in
      (VertexMap.add v_id ~key:v ~data:node_id, IntMap.add id_v ~key:node_id ~data:v))

let port_to_pnk port =
    match Int32.to_int port with
    | None -> failwith "Invalid port"
    | Some x -> x

(* Topology to ProbNetKAT program *)
let topo_to_pnk (topo : Topology.t) (v_id, id_v) =
    Topology.fold_edges (fun link pol_acc ->
    let src, src_port = Topology.edge_src link in
    let dst, dst_port = Topology.edge_dst link in
    let src_id = VertexMap.find_exn v_id src in
    let dst_id = VertexMap.find_exn v_id dst in
    let link_pnk = ??(Switch src_id) >> ??(Port (port_to_pnk src_port)) >> Dup
                 >> !!(Switch dst_id) >> !!(Port (port_to_pnk dst_port)) >> Dup in
    if pol_acc = Drop then link_pnk
    else link_pnk & pol_acc) topo Drop

(* Topology with lossy links to ProbNetKAT program *)
let topo_to_pnk_lossy (topo : Topology.t) (fail_prob : Prob.t) (v_id, id_v) =
    Topology.fold_edges (fun link pol_acc ->
    let src, src_port = Topology.edge_src link in
    let dst, dst_port = Topology.edge_dst link in
    let src_id = VertexMap.find_exn v_id src in
    let dst_id = VertexMap.find_exn v_id dst in
    let link_src_test = ??(Switch src_id) >> ??(Port (port_to_pnk src_port)) in
    let link_dest_mod =  !!(Switch dst_id) >> !!(Port (port_to_pnk dst_port)) in
    let link_prog = link_src_test >> Dup >>
      ?@[(link_dest_mod >> Dup, Prob.(one - fail_prob));
        (Drop, fail_prob)] in
    if pol_acc = Drop then link_prog
    else link_prog & pol_acc) topo Drop

(* ProbNetKAT program to test if a packet is at a host *)
let test_pkt_at_host (topo : Topology.t) (v_id, id_v) =
  get_hosts_set topo
  |> VertexSet.fold ~init:Drop
    ~f:(fun pol_acc host -> pol_acc & ??(Switch (VertexMap.find_exn v_id host)))

(* ProbNetKAT program to test if a packet is at network edge (host & port) *)
let test_pkt_at_edge (topo : Topology.t) (v_id, id_v) =
  get_hosts_set topo
  |> VertexSet.fold ~init:Drop
    ~f:(fun acc v ->
      let host_id = VertexMap.find_exn v_id v in
      VertexSet.fold (Topology.neighbors topo v)
        ~init:acc
        ~f:(fun acc neigh ->
          let link = Topology.find_edge topo v neigh in
          let port_id = port_to_pnk (snd (Topology.edge_src link)) in
          let test_pol = ??(Switch host_id) >> ??(Port port_id) in
          if acc = Drop then test_pol
          else acc & test_pol))


(****************************** Routing Schemes *******************************)
(************ Global Path based *******************)
(* Topology -> SPF source_routes *)
let route_spf (topo : Topology.t) : source_routes =
  let device v = vertex_to_label topo v
    |> Node.device in
  let apsp = NetPath.all_pairs_shortest_paths
    ~topo:topo
    ~f:(fun x y ->
        match (device x, device y) with
        | (Node.Host,Node.Host) -> true
        | _ -> false) in
  List.fold_left apsp
    ~init:SrcDstMap.empty
    ~f:(fun acc (c,v1,v2,p) ->
      SrcDstMap.add acc ~key:(v1,v2) ~data:(PathMap.singleton p Prob.one))

(* Compute source_routes with k shortest paths between hosts *)
let route_ksp (topo : Topology.t) (k : int) : source_routes =
  let host_set = get_hosts_set topo in
  all_pair_ksp topo k host_set host_set
    |> SrcDstMap.fold ~init:SrcDstMap.empty
      ~f:(fun ~key:(v1,v2) ~data:paths acc ->
        if (v1 = v2) then acc else
        let path_map = List.fold_left paths ~init:PathMap.empty
          ~f:(fun acc path ->
            PathMap.add acc ~key:path ~data:Prob.(one / of_int (List.length paths))) in
        SrcDstMap.add acc ~key:(v1,v2) ~data:path_map)

(* Compute source_routes with Raecke's routing algorithm *)
let route_raecke (topo : Topology.t) (d : Prob.t SrcDstMap.t) : source_routes =
  let float_d = SrcDstMap.map d ~f:(Prob.to_float) in
  solve_raecke topo float_d

(************ Local switch based *******************)
(* First outgoing port for a path *)
let get_out_port path =
  match path with
  | link::_ -> (snd (Topology.edge_src link))
  | [] -> failwith "Empty edge list"

(* Topology -> hop by hop scheme using shortest path *)
let route_per_hop_spf (topo : Topology.t) : per_hop_routes =
  let device v = vertex_to_label topo v
    |> Node.device in
  let apsp = NetPath.all_pairs_shortest_paths
    ~topo:topo
    ~f:(fun x y ->
        match (device x, device y) with
        | (_, Node.Host) -> true
        | _ -> false) in
  List.fold_left apsp
    ~init:SrcDstMap.empty
    ~f:(fun acc (c,v1,v2,p) ->
      if v1 = v2 then acc
      else SrcDstMap.add acc ~key:(v1,v2)
        ~data:(PortMap.singleton (port_to_pnk (get_out_port p)) Prob.one))

(* Topology -> hop by hop state scheme using all shortest paths ECMP *)
let route_per_hop_ecmp (topo : Topology.t) : per_hop_routes =
  all_pairs_multi_shortest_path topo
  |> SrcDstMap.fold ~init:SrcDstMap.empty
    ~f:(fun ~key:(src, dst) ~data:(_,n,next_hop_probs) acc ->
      if src = dst || is_switch topo dst then acc
      else
        let next_hops = List.fold_left next_hop_probs
          ~init:VertexSet.empty
          ~f:(fun acc (next_hop,_) ->
            VertexSet.add acc next_hop) in
        let prob = Prob.(one / of_int (VertexSet.length next_hops)) in
        (* Every out port on a shortest path gets equal weight *)
        let out_port_probs = VertexSet.fold next_hops
          ~init:PortMap.empty
          ~f:(fun acc next_hop ->
            let link = Topology.find_edge topo src next_hop in
            let out_port = port_to_pnk(snd (Topology.edge_src link)) in
            PortMap.add acc ~key:out_port ~data:prob) in
        SrcDstMap.add acc ~key:(src, dst) ~data:out_port_probs)

(* Compute hop by hop routing scheme with k shortest paths from a node to a host *)
let route_per_hop_ksp (topo : Topology.t) (k : int) : per_hop_routes =
  let host_set = get_hosts_set topo in
  let all_nodes_set = Topology.vertexes topo in
  all_pair_ksp topo k all_nodes_set host_set
  |> SrcDstMap.fold ~init:SrcDstMap.empty
    ~f:(fun ~key:(v1,v2) ~data:paths acc ->
      if (v1 = v2) then acc
      else
        let prob = Prob.(one / of_int (List.length paths)) in
        (* out ports have weights proportional to number of paths through them *)
        let out_port_probs = List.fold_left paths ~init:PortMap.empty
          ~f:(fun acc path ->
            let out_port = port_to_pnk (get_out_port path) in
            let existing_prob = match PortMap.find acc out_port with
              | None -> Prob.zero
              | Some x -> x in
            PortMap.add acc ~key:out_port ~data:Prob.(existing_prob + prob)) in
      SrcDstMap.add acc ~key:(v1,v2) ~data:out_port_probs)

(* Compute hop by hop routing scheme for random walk *)
let route_per_hop_random_walk (topo : Topology.t) : per_hop_routes =
  let host_set = get_hosts_set topo in
  let switch_set = get_switch_set topo in
  VertexSet.fold switch_set ~init:SrcDstMap.empty
    ~f:(fun acc sw ->
      let neighs = Topology.neighbors topo sw in
      VertexSet.fold host_set ~init:acc
        ~f:(fun acc dst ->
          if VertexSet.mem neighs dst then
            begin
              (* if dst is a neighbor, then send directly *)
              let link = Topology.find_edge topo sw dst in
              let out_port = port_to_pnk (snd (Topology.edge_src link)) in
              let out_port_probs = PortMap.singleton out_port Prob.one in
              SrcDstMap.add acc ~key:(sw,dst) ~data:out_port_probs
            end
          else
            begin
              (* else forward to random next hop *)
              let sw_neighs = VertexSet.filter neighs ~f:(is_switch topo) in
              let prob = Prob.(one / of_int (VertexSet.length sw_neighs)) in
              let out_port_probs = VertexSet.fold sw_neighs
                ~init:PortMap.empty
                ~f:(fun acc n_sw ->
                  let link = Topology.find_edge topo sw n_sw in
                  let out_port = port_to_pnk (snd (Topology.edge_src link)) in
                  let existing_prob = match PortMap.find acc out_port with
                    | None -> Prob.zero
                    | Some x -> x in
                  PortMap.add acc ~key:out_port ~data:Prob.(existing_prob + prob)) in
              SrcDstMap.add acc ~key:(sw,dst) ~data:out_port_probs
            end))

(************** Translate routing schemes to ProbNetKAT policies **************)

(* translate a path as a global probnetkat program that includes switch and link actions *)
let path_to_global_pnk (topo : Topology.t) (path : path) (v_id, id_v) =
  let _, pol = List.fold_left path
    ~init:(None, None)
    ~f:(fun (switch_inport, pol) link ->
      let src_sw, src_pt = Topology.edge_src link in
      let dst_sw, dst_pt = Topology.edge_dst link in
      let src_sw_id = VertexMap.find_exn v_id src_sw in
      let src_pt_id = port_to_pnk src_pt in
      let dst_sw_id = VertexMap.find_exn v_id dst_sw in
      let dst_pt_id = port_to_pnk dst_pt in
      let link_pol = (* src switch out-port -> dst switch in-port *)
        ??(Switch src_sw_id) >> ??(Port src_pt_id) >> Dup >>
        !!(Switch dst_sw_id) >> !!(Port dst_pt_id) >> Dup in
      let new_pol = match switch_inport with
        | None -> link_pol
        | Some (in_sw_id, in_pt_id) ->
            begin
              (* policy at switch : src switch in-port -> src switch out-port *)
              let curr_sw_pol =
                ??(Switch  in_sw_id) >> ??(Port  in_pt_id) >>
                !!(Switch src_sw_id) >> !!(Port src_pt_id) in
              match pol with
                | None -> curr_sw_pol >> link_pol
                | Some p -> p >> curr_sw_pol >> link_pol
            end in
      (Some (dst_sw_id, dst_pt_id), Some new_pol)) in
  match pol with
  | None -> Drop
  | Some p -> p

(* translate a source_routes scheme as a global probnetkat program that includes
 * switch and link actions *)
let source_routes_to_global_pnk (topo : Topology.t) (routes : source_routes) (v_id, id_v) =
  SrcDstMap.fold routes ~init:Drop
    ~f:(fun ~key:(src,dst) ~data:path_dist pol_acc ->
      if src = dst then pol_acc
      else
        let src_id = VertexMap.find_exn v_id src in
        let dst_id = VertexMap.find_exn v_id dst in
        let path_choices = PathMap.fold path_dist ~init:[]
          ~f:(fun ~key:path ~data:prob acc ->
            (path_to_global_pnk topo path (v_id, id_v), prob)::acc) in
        let sd_route_pol = ??(Src src_id) >> ??(Dst dst_id) >> ?@path_choices in
        if pol_acc = Drop then sd_route_pol
        else pol_acc & sd_route_pol)


let union_pol_opt p q : Pol.t option =
  match (p, q) with
    | (None, None) -> None
    | (Some _, None) -> p
    | (None, Some _) -> q
    | (Some p', Some q') -> Some (p' & q')


(* translate a path as a local probnetkat program by tagging pkts *)
let path_to_local_pnk v_id tag path : Pol.t option =
  match path with
    | _::(_::links) ->
      (* ignore the first two links as the first hop will set the tag *)
      List.fold_left links ~init:None ~f:(fun pol_acc link ->
        let sw, pt = Topology.edge_src link in
        let sw_id = VertexMap.find_exn v_id sw in
        let pt_id = port_to_pnk pt in
        let sw_pol = ??(Switch sw_id) >> ??(Tag tag) >> !!(Port pt_id) in
        union_pol_opt pol_acc (Some sw_pol))
    | _ -> None

(* translate a source_routes scheme as a local probnetkat program by tagging pkts *)
let source_routes_to_local_pnk (topo : Topology.t) (routes : source_routes) (v_id, id_v) =
  let tag = ref 0 in
  let routes_pnk = SrcDstMap.fold routes ~init:None
    ~f:(fun ~key:(src,dst) ~data:path_dist pol_acc ->
      if src = dst then pol_acc
      else
        let src_id = VertexMap.find_exn v_id src in
        let dst_id = VertexMap.find_exn v_id dst in
        let set_tag_action, path_pol = PathMap.fold path_dist ~init:([], pol_acc)
          ~f:(fun ~key:path ~data:prob (tag_prob, pol_acc) ->
            tag := !tag + 1;
            let path_pnk = path_to_local_pnk v_id !tag path in (* rest of path matches on tag and forwards *)
            (* ingress switch sets the tag and forwards on corresponding port *)
            let ingress_sw_out_pt = (List.nth path 1)
              |> get_opt_exn
              |> Topology.edge_src
              |> snd
              |> port_to_pnk in
            ( (!!(Tag !tag) >> !!(Port ingress_sw_out_pt), prob)::tag_prob,
              union_pol_opt path_pnk pol_acc )) in
        let ingress_sws = PathMap.fold path_dist ~init:VertexSet.empty ~f:(fun ~key:path ~data:_ acc ->
          match List.hd path with
          | None -> acc
          | Some x -> Topology.edge_dst x |> fst |> VertexSet.add acc) in
        if VertexSet.length ingress_sws > 1 then
          failwith "Host connected to multiple switches"
        else
          let ingress_sw_id = VertexMap.find_exn v_id (VertexSet.choose ingress_sws |> get_opt_exn) in
          let set_tag_pol = ??(Switch ingress_sw_id) >> ??(Src src_id) >> ??(Dst dst_id) >> ?@set_tag_action in
          union_pol_opt (Some set_tag_pol) path_pol) in
  match routes_pnk with
    | None -> Drop
    | Some p -> p

(* translate a hop by hop routing scheme as a local probnetkat program that
 * includes only switch actions *)
let per_hop_routes_to_local_pnk (topo : Topology.t) (routes : per_hop_routes) (v_id, id_v) =
  SrcDstMap.fold routes ~init:Drop
    ~f:(fun ~key:(src,dst) ~data:port_dist pol_acc ->
      if src = dst || is_host topo src || is_switch topo dst then pol_acc
      else
        let src_id = VertexMap.find_exn v_id src in
        let dst_id = VertexMap.find_exn v_id dst in
        let port_choices = PortMap.fold port_dist ~init:[]
          ~f:(fun ~key:port ~data:prob acc ->
            (!!(Port port), prob)::acc) in
        let local_sw_pol = ??(Switch src_id) >> ??(Dst dst_id) >> ?@port_choices in
        if pol_acc = Drop then local_sw_pol
        else pol_acc & local_sw_pol)

(******************* Read traffic demand distribution *************************)
(** Assuming all demands are integers in Mbps **)
let read_demands (dem_file : string) (topo : Topology.t) : (Prob.t SrcDstMap.t) =
  (* Output is a src-dst map of demands in Mbps *)
  let str_node_map =
    Topology.fold_vertexes
    (fun v acc ->
      StringMap.add acc ~key:(Node.name (vertex_to_label topo v)) ~data:v) topo StringMap.empty in
  In_channel.with_file dem_file
    ~f:(fun file ->
      In_channel.fold_lines file ~init:SrcDstMap.empty
        ~f:(fun acc line ->
          let entries = Array.of_list (String.split line ~on:' ') in
          let src = StringMap.find_exn str_node_map entries.(0) in
          let dst = StringMap.find_exn str_node_map entries.(1) in
          let dem = Prob.((of_int (int_of_string entries.(2))) * of_int 1048576) in (*Mbps to bps*)
          SrcDstMap.add acc ~key:(src,dst) ~data:dem))

(* Convert demands map to a probability distribution, also store the aggregate demand *)
let inp_dist_to_pnk (demands : Prob.t SrcDstMap.t) (v_id, id_v) : (Prob.t * Pol.t) =
  let agg_dem = SrcDstMap.fold demands ~init:Prob.zero
    ~f:(fun ~key:_ ~data:dem acc -> Prob.(dem + acc)) in
  let dist = SrcDstMap.fold demands ~init:[]
    ~f:(fun ~key:(s,d) ~data:dem acc ->
      let src_id = VertexMap.find_exn v_id s in
      let dst_id = VertexMap.find_exn v_id d in
      (!!(Switch src_id) >> !!(Port 1) >> !!(Src src_id) >> !!(Dst dst_id), Prob.(dem / agg_dem))::acc) in
  (agg_dem, ?@dist)

(************************ ProbNetKAT queries **********************************)
(* measure number of hops *)
let path_length (pk,h) =
  Prob.(of_int (List.length h ) / of_int 2 - of_int 1)

let num_packets (pk,h) = Prob.one

(* measure congestion or utilization of a link due to a flow *)
let link_congestion topo (v_id, id_v) agg_dem link (pk, h) : Prob.t =
  let rec hist_traverses_link (lsrc, ldst) hist : bool =
    (* true if traverses, false otherwise *)
    match hist with
    | s1::(s2::_ as t) ->
        if (s1.Pkt.switch = Some ldst && s2.Pkt.switch = Some lsrc) then true
        else hist_traverses_link (lsrc, ldst) t
    | _ -> false in
  let link_src_id = VertexMap.find_exn v_id (fst (Topology.edge_src link)) in
  let link_dst_id = VertexMap.find_exn v_id (fst (Topology.edge_dst link)) in
  if hist_traverses_link (link_src_id, link_dst_id) h then
    let link_cap = Topology.edge_to_label topo link
      |> Link.capacity
      |> Prob.of_int64 in
    Prob.(agg_dem / link_cap)
  else Prob.zero

(* Measure throughput between a pair of hosts *)
let sd_tput topo (v_id, id_v) agg_dem (src, dst) (pk, h) : Prob.t =
  let src_id = VertexMap.find_exn v_id src in
  let dst_id = VertexMap.find_exn v_id dst in
  (* check pkt's src and dst and verify the pkt has reached dst *)
  if (pk.Pkt.src = Some src_id && pk.Pkt.dst = Some dst_id && pk.Pkt.switch = Some dst_id) then agg_dem
  else Prob.zero

(* Test loops by checking if a pkt appears more than once in history *)
let test_loop_history (pk, h) : Prob.t =
  let pkts = List.fold_left h ~init:PktSet.empty
    ~f:(fun acc pk ->
      PktSet.add acc pk) in
  if List.length h = PktSet.length pkts then Prob.zero
  else Prob.one

(* query on a hist ---> query on hist set by taking average *)
let lift_query_avg q = fun hset ->
  let n = HSet.length hset in
  if n = 0 then Prob.zero else
  let sum = HSet.fold hset ~init:Prob.zero ~f:(fun acc h ->
    Prob.(acc + q h))
  in
  Prob.(sum / of_int n)

(* query on a hist ---> query on hist set by taking sum *)
let lift_query_sum q = fun hset ->
  HSet.fold hset ~init:Prob.zero ~f:(fun acc h -> Prob.(acc + q h))

(************************************* Tests **********************************)

(* run f for with i iterations and apply f on output dist to calculate required metric *)
let metric_convergence ~(p:Pol.t) ~(f:Dist.t -> Prob.t) ~(n:int) =
  let rec range i j = if i >= j then [] else i :: (range (i+1) j) in
  let iterations = range 1 n in
  List.fold_left iterations ~init:IntMap.empty
    ~f:(fun acc i -> IntMap.add acc ~key:i ~data:(eval i p |> f))

(* Input: topology, routing policy, input distribution. Aggregate throughput vs number of star iterations *)
let net_tput_convergence (topo: Topology.t) (vertex_id_bimap) (network_pnk : Pol.t)
                        (input_dist : Prob.t SrcDstMap.t) (output_filter : Pol.t) =
  let agg_dem, input_dist_pnk = inp_dist_to_pnk input_dist vertex_id_bimap in
  let p = input_dist_pnk >> network_pnk >> output_filter in
  metric_convergence ~p ~f:(fun out_dist ->
    SrcDstMap.fold input_dist ~init:Prob.zero
      ~f:(fun ~key:(src, dst) ~data:_ acc ->
        let tput = Dist.expectation out_dist
          ~f:(lift_query_sum (sd_tput topo vertex_id_bimap agg_dem (src, dst))) in
        Prob.(acc + tput / agg_dem))) ~n:10


(* Input: topology, routing policy, input distribution. Measure expectations of a few metrics *)
let analyze_routing_scheme (topo: Topology.t) (vertex_id_bimap) (network_pnk : Pol.t)
      (input_dist : Prob.t SrcDstMap.t) (output_filter : Pol.t) =
  let agg_dem, input_dist_pnk = inp_dist_to_pnk input_dist vertex_id_bimap in
  let p = input_dist_pnk >> network_pnk >> output_filter in
  let out_dist = eval 10 p in
  (*Dist.print out_dist;*)
  (* Measure latency in terms of path length. Ignore empty hist sets when calculating expectation *)
  expectation_normalized out_dist ~f:(lift_query_avg path_length)
    |> Prob.to_dec_string
    |> Printf.printf "Latency:\t%s\n";

  (* Measure number of packets *)
  Dist.expectation out_dist ~f:(lift_query_sum num_packets)
    |> Prob.to_dec_string
    |> Printf.printf "Num packets:\t%s\n";

  (* Check loops in output distribution *)
  Dist.expectation out_dist ~f:(lift_query_sum test_loop_history)
    |> fun x -> Prob.(x = zero)
    |> Printf.printf "Loop free:\t%b%!\n";

  (* measure link congestions *)
  let link_congestions = Topology.fold_edges
    (fun link acc ->
      let cong = Dist.expectation out_dist
        ~f:(lift_query_sum (link_congestion topo vertex_id_bimap agg_dem link)) in
      EdgeMap.add acc ~key:link ~data:cong) topo EdgeMap.empty in
  (*EdgeMap.iteri link_congestions ~f:(fun ~key:link ~data:cong ->
    Printf.printf "%s : %s\n" (string_of_edge topo link) (Prob.to_dec_string cong))*)

  (* measure throughput per src dst pair *)
  Printf.printf "Throughput per src-dest\n";
  let sd_tputs = SrcDstMap.fold ~init:SrcDstMap.empty
    ~f:(fun ~key:(src, dst) ~data:_ acc ->
      let tput = Dist.expectation out_dist
        ~f:(lift_query_sum (sd_tput topo vertex_id_bimap agg_dem (src, dst))) in
      SrcDstMap.add acc ~key:(src, dst) ~data:tput) input_dist in
  SrcDstMap.iteri sd_tputs ~f:(fun ~key:(s,d) ~data:tput ->
    Printf.printf "(%s,%s) : %s / %s\n" (string_of_vertex topo s) (string_of_vertex topo d)
      Prob.(to_dec_string (tput / of_int 1048576))
      Prob.(to_dec_string (SrcDstMap.find_exn input_dist (s,d) / of_int 1048576)));

  (*maximum congestion*)
  let max_cong = EdgeMap.fold ~init:Prob.zero
   ~f:(fun ~key:link ~data:cong acc -> Prob.max acc cong) link_congestions in
  Printf.printf "Max Congestion:\t%s\n" (Prob.to_dec_string max_cong);
  Printf.printf "Congestion drop:\t%b\n" Prob.(max_cong > Prob.one);
  net_tput_convergence topo vertex_id_bimap network_pnk input_dist output_filter

(* Check loops by sending in wildcarded packets *)
let check_loops (network_pnk: Pol.t) (set_ingress_location: Pol.t)=
  (*Dist.print (eval 20 (set_ingress_location >> network_pnk));*)
  expectation' 5 (set_ingress_location >> network_pnk) ~f:test_loop_history
    |> Prob.to_dec_string
    |> Printf.printf "Loops present: %s\n%!"

(* Measure a few performance statistics for global path-based routing schemes *)
let test_global topo_file dem_file = begin
  pprintf red "\n==== Global path based routing ====\n";
  let topo = Parse.from_dotfile topo_file in
  let vertex_id_bimap = gen_node_pnk_id_map topo in
  let input_dist = read_demands dem_file topo in
  let routing_cases = StringMap.empty
    |> StringMap.add ~key:"SPF centralized - path based"  ~data:(route_spf topo)
    |> StringMap.add ~key:"KSP centralized - path based" ~data:(route_ksp topo 2)
    |> StringMap.add ~key:"Raecke centralized - path based" ~data:(route_raecke topo input_dist) in
  StringMap.iteri routing_cases ~f:(fun ~key:name ~data:scheme ->
    pprintf red ("\n*** "^ name ^" ***\n");
    let routes_pnk = source_routes_to_global_pnk topo scheme vertex_id_bimap in
    let conv = analyze_routing_scheme topo vertex_id_bimap routes_pnk input_dist Id in
    dump_to_file name "#iter\ttput" conv)
end

(* Measure a few performance statistics for local link-based routing schemes *)
let test_local topo_file dem_file = begin
  pprintf red "\n==== Local next-hop based routing ====\n";
  let topo = Parse.from_dotfile topo_file in
  let vertex_id_bimap = gen_node_pnk_id_map topo in
  let topo_pnk = topo_to_pnk topo vertex_id_bimap in
  (*let topo_pnk = topo_to_pnk_lossy topo Prob.(of_int 1/ of_int 10) vertex_id_bimap in*)
  let test_at_host_pnk = test_pkt_at_host topo vertex_id_bimap in
  let input_dist = read_demands dem_file topo in
  let routing_cases = StringMap.empty
    |> StringMap.add ~key:"SPF distributed hop-by-hop"  ~data:(route_per_hop_spf topo)
    |> StringMap.add ~key:"ECMP distributed hop-by-hop" ~data:(route_per_hop_ecmp topo)
    |> StringMap.add ~key:"KSP distributed hop-by-hop"  ~data:(route_per_hop_ksp topo 2)
    |> StringMap.add ~key:"Random walk distributed hop-by-hop"  ~data:(route_per_hop_random_walk topo) in
  StringMap.iteri routing_cases ~f:(fun ~key:name ~data:ls_scheme ->
    pprintf red ("\n*** "^ name ^" ***\n");
    let routes_pnk = per_hop_routes_to_local_pnk topo ls_scheme vertex_id_bimap in
    let network_pnk = (Star (topo_pnk >> routes_pnk)) >> topo_pnk in
    let conv = analyze_routing_scheme topo vertex_id_bimap network_pnk input_dist test_at_host_pnk in
    dump_to_file name "#iter\ttput" conv);
  let routing_cases = StringMap.empty
    |> StringMap.add ~key:"SPF centralized hop-by-hop" ~data:(route_spf topo)
    |> StringMap.add ~key:"KSP centralized hop-by-hop"  ~data:(route_ksp topo 2)
    |> StringMap.add ~key:"Raecke centralized hop-by-hop" ~data:(route_raecke topo input_dist) in
  StringMap.iteri routing_cases ~f:(fun ~key:name ~data:ls_scheme ->
    pprintf red ("\n*** "^ name ^" ***\n");
    let routes_pnk = source_routes_to_local_pnk topo ls_scheme vertex_id_bimap in
    let network_pnk = (Star (topo_pnk >> routes_pnk)) >> topo_pnk in
    let conv = analyze_routing_scheme topo vertex_id_bimap network_pnk input_dist test_at_host_pnk in
    dump_to_file name "#iter\ttput" conv);
  (*Printf.printf "test at edge: %s\n%!" (Pol.to_string test_at_edge_pnk);
  let test_at_edge_pnk = test_pkt_at_edge topo vertex_id_bimap in
  check_loops ((Star (topo_pnk >> spf_pnk)) >> topo_pnk) test_at_edge_pnk;*)
end

let () =
  let topology = "4cycle" in
  let topo_file = "examples/" ^ topology ^ ".dot" in
  let dem_file = "examples/" ^ topology ^ ".dem" in
  test_local topo_file dem_file;
  test_global topo_file dem_file

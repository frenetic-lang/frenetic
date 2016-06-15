open Core.Std
open Frenetic_Network
open Frenetic_ProbNetKAT_Interpreter
open Net
open Num
open Topology

module PQueue = Core_kernel.Heap.Removable
include Interp(Hist)(PreciseProb)

open Pol

module VertexOrd = struct
  type t = Topology.vertex [@@deriving sexp]
  let compare = Pervasives.compare
end

module VertexMap = Map.Make(VertexOrd)
module IntMap = Map.Make(Int)
module StringMap = Map.Make(String)

module SrcDstOrd = struct
  type t = Topology.vertex * Topology.vertex [@@deriving sexp]
  let compare = Pervasives.compare
end

module SrcDstMap = Map.Make(SrcDstOrd)

type path = Topology.edge list [@@deriving sexp]

module PathOrd = struct
  type t = path [@@deriving sexp]
  let compare = Pervasives.compare
end

module PathMap = Map.Make(PathOrd)

type flow_decomp = num PathMap.t
type scheme = flow_decomp SrcDstMap.t

(* helper functions *)

let intercalate f s = function
  | [] ->
      ""
  | h::t ->
      List.fold_left t ~f:(fun acc x -> acc ^ s ^ f x) ~init:(f h)

let dump_edges (t:Topology.t) (es:path) : string =
  intercalate
    (fun e ->
     Printf.sprintf "(%s,%s)"
        (Node.name (Net.Topology.vertex_to_label t (fst (Net.Topology.edge_src e))))
        (Node.name (Net.Topology.vertex_to_label t (fst (Net.Topology.edge_dst e))))) ", "  es

let dump_path_prob_set (t:Topology.t) (pps: flow_decomp) : string =
  let buf = Buffer.create 101 in
  PathMap.iteri
    pps
    ~f:(fun ~key:path ~data:prob ->
      Printf.bprintf buf "[%s] @ %f\n" (dump_edges t path) (float_of_num prob));
  Buffer.contents buf

let dump_scheme (t:Topology.t) (s:scheme) : string =
  let buf = Buffer.create 101 in
  SrcDstMap.iteri s
    ~f:(fun ~key:(v1,v2) ~data:pps ->
      Printf.bprintf buf "%s -> %s :\n  %s\n"
          (Node.name (Net.Topology.vertex_to_label t v1))
          (Node.name (Net.Topology.vertex_to_label t v2))
          (dump_path_prob_set t pps));
  Buffer.contents buf

let get_hosts_set (topo : Topology.t) : VertexSet.t =
  VertexSet.filter (Topology.vertexes topo)
  ~f:(fun v ->
    let label = Topology.vertex_to_label topo v in
    Node.device label = Node.Host)

(******************* Topology *******************)
let is_host (topo : Topology.t) (node : vertex) =
  Node.device (vertex_to_label topo node) = Node.Host

let gen_node_pnk_id_map (topo : Topology.t) =
  let host_offset =
    Int.of_float (10. ** (ceil (log10 (Float.of_int (num_vertexes topo))) +. 1.)) in
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

let topo_to_pnk (topo : Topology.t) (v_id, id_v) =
  (* Topology dot file to ProbNetKAT program *)
    fold_edges (fun edge pol_acc ->
    let src, src_port = edge_src edge in
    let dst, dst_port = edge_dst edge in
    let src_id = VertexMap.find_exn v_id src in
    let dst_id = VertexMap.find_exn v_id dst in
    let edge_src_test = ??(Switch src_id) >> ??(Port (port_to_pnk src_port)) in
    let edge_dest_mod =  !!(Switch dst_id) >> !!(Port (port_to_pnk dst_port)) in
    let edge_prog = edge_src_test >> Dup >> edge_dest_mod >> Dup in
    if pol_acc = Drop then
      edge_prog
    else
      edge_prog & pol_acc)
    topo Drop

(******************* Routing Schemes *******************)

(* Topology -> scheme *)
let route_spf (topo : Topology.t) : scheme =
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
      SrcDstMap.add acc ~key:(v1,v2) ~data:(PathMap.singleton p (num_of_int 1)))

let remove_cycles path =
  let edge_dst t = fst (Topology.edge_dst t) in
  let edge_src t = fst (Topology.edge_src t) in
  let path_segment path v = match path with
        | [] -> []
        | h::t ->
            fst (List.fold_right path ~init:([], false) ~f:(fun next (acc,over) ->
              if over then (acc,over)
              else
                let src = edge_src next in
                let dst = edge_dst next in
                if src = v then (acc,true)
                else if dst = v then (next::acc, true) else (next::acc, false))) in
  let rec check path acc seen = match path with
        | [] -> acc
        | h::t ->
            let dst = edge_dst h in
            if Topology.VertexSet.mem seen dst then
              (* cut out the cycle *)
              let segment = path_segment acc dst in
              let first_node = match segment with
              | [] -> Topology.VertexSet.empty
              | h::t -> Topology.VertexSet.singleton (edge_src h) in
              let new_seen = List.fold_left segment ~init:first_node ~f:(fun acc next ->
                Topology.VertexSet.add acc (edge_dst next)) in
              check t segment new_seen
                else
                  check t (h::acc) (Topology.VertexSet.add seen dst) in
  match path with
        | [] -> []
        | h::t -> let first_src = (edge_src h) in
        List.rev (check path [] (Topology.VertexSet.singleton first_src))


let k_shortest_path (topo : Topology.t) (s : Topology.vertex) (t : Topology.vertex) (k : int) =
  (* k-shortest s,t paths - loops can be present *)
  if s = t then [] else
  let paths = ref [] in
  let count = Hashtbl.Poly.create () in
  Topology.iter_vertexes
    (fun u ->
     Hashtbl.Poly.add_exn count u 0;
    ) topo;

  let bheap = PQueue.create
                  ~min_size:(Topology.num_vertexes topo)
                  ~cmp:(fun (dist1,_) (dist2,_) -> Float.compare dist1 dist2) () in

  let _ = PQueue.add_removable bheap (0.0, [s]) in
  let rec explore () =
    let cost_path_u = PQueue.pop bheap in
    match cost_path_u with
    | None -> ()
    | Some (cost_u, path_u) ->
        match List.hd path_u with (* path_u contains vertices in reverse order *)
        | None -> ()
        | Some u ->
          let count_u = Hashtbl.Poly.find_exn count u in
          let _  = Hashtbl.Poly.set count u (count_u + 1) in
          if u = t then paths := List.append !paths [path_u];
          if count_u < k then
            (* if u = t then explore()
            else *)
            let _ = Topology.iter_succ
            (fun edge ->
              let (v,_) = Topology.edge_dst edge in
              if (List.mem path_u v) then () else (* consider only simple paths *)
              let path_v = v::path_u in
              let weight = Link.weight (Topology.edge_to_label topo edge) in
              let cost_v = cost_u +. weight in
              let _ = PQueue.add_removable bheap (cost_v, path_v) in
              ()) topo u in
            explore ()
          else if u = t then ()
          else explore () in
  explore ();
  List.fold_left
    !paths
    ~init:[]
    ~f:(fun acc path ->
      let edge_path = List.fold_left path
      ~init:([], None)
      ~f:(fun edges_u v ->
        let edges, u = edges_u in
        match u with
        | None -> (edges, Some v)
        | Some u -> let edge = Topology.find_edge topo v u in (edge::edges, Some v)) in
      let p,_ = edge_path in
      (remove_cycles p)::acc)

let all_pair_ksp (topo : Topology.t) (k : int) hosts =
  VertexSet.fold hosts
  ~init:SrcDstMap.empty
  ~f:(fun acc src ->
        VertexSet.fold hosts ~init:acc
        ~f:(fun nacc dst ->
          let ksp = k_shortest_path topo src dst k in
          SrcDstMap.add nacc ~key:(src, dst) ~data:ksp))

let route_ksp (topo : Topology.t) (k : int) : scheme =
  let host_set = get_hosts_set topo in
  let all_ksp = all_pair_ksp topo k host_set in
  SrcDstMap.fold all_ksp ~init:SrcDstMap.empty
    ~f:(fun ~key:(v1,v2) ~data:paths acc ->
      if (v1 = v2) then acc
      else
      let path_map = List.fold_left
          paths
          ~init:PathMap.empty
          ~f:(fun acc path ->
              let prob = (num_of_int 1) // num_of_int (List.length paths) in
              PathMap.add acc ~key:path ~data:prob) in
      SrcDstMap.add acc ~key:(v1,v2) ~data:path_map)

(**** Translate routing schemes to ProbNetKAT policies *****)
let path_to_pnk (topo : Topology.t) (path : path) (include_links : bool) (v_id, id_v) =
  (* translate a path *)
  let (_,_), pol = List.fold_left path
    ~init:((0,0), Drop)
    ~f:(fun ((in_sw_id, in_pt_id), pol) edge ->
      let src_sw, src_pt = edge_src edge in
      let dst_sw, dst_pt = edge_dst edge in
      let src_sw_id = VertexMap.find_exn v_id src_sw in
      let src_pt_id = port_to_pnk src_pt in
      let dst_sw_id = VertexMap.find_exn v_id dst_sw in
      let dst_pt_id = port_to_pnk dst_pt in
      let edge_pol = (* src_port -> dst_port *)
        ??(Switch src_sw_id) >> ??(Port src_pt_id) >> Dup >>
        !!(Switch dst_sw_id) >> !!(Port dst_pt_id) >> Dup in
      let new_pol =
        if in_sw_id = 0 && in_pt_id = 0 then
          if include_links then edge_pol
          else pol
        else
          let curr_sw_pol = (* policy at switch : dst_port of prev edge to src_port of curr edge *)
            ??(Switch  in_sw_id) >> ??(Port  in_pt_id) >>
            !!(Switch src_sw_id) >> !!(Port src_pt_id) in
          if pol = Drop && include_links then curr_sw_pol >> edge_pol
          else if pol = Drop && (not include_links) then curr_sw_pol
          else if pol <> Drop && include_links then pol >> curr_sw_pol >> edge_pol
          else pol & curr_sw_pol in
      ((dst_sw_id, dst_pt_id), new_pol)) in
      pol

let routing_scheme_to_pnk (topo : Topology.t) (routes : scheme) (include_links : bool) (v_id, id_v) =
  (* translate a routing scheme *)
  SrcDstMap.fold routes ~init:Drop
    ~f:(fun ~key:(src,dst) ~data:path_dist pol_acc ->
      if src = dst then
        pol_acc
      else
        let src_id = VertexMap.find_exn v_id src in
        let dst_id = VertexMap.find_exn v_id dst in
        let path_choices =
          PathMap.fold path_dist ~init:[]
            ~f:(fun ~key:path ~data:prob acc ->
              (path_to_pnk topo path include_links (v_id, id_v), prob)::acc) in
        let sd_route_pol =
          ??(Src src_id) >> ??(Dst dst_id) >> ?@path_choices in
        if pol_acc = Drop then
          sd_route_pol
        else
          pol_acc & sd_route_pol)

(**** ProbNetKAT queries ****)
let path_length (pk,h) = (Prob.of_int (List.length h )) // (Prob.of_int 2)
let num_packets (pk,h) = Prob.of_int 1
let inp_dist_3eq = ?@[ !!(Switch 101) >> !!(Port 1) >> !!(Src 101) >> !!(Dst 102), 1/6
                ;  !!(Switch 101) >> !!(Port 1) >> !!(Src 101) >> !!(Dst 103), 1/6
                ;  !!(Switch 102) >> !!(Port 1) >> !!(Src 102) >> !!(Dst 101), 1/6
                ;  !!(Switch 102) >> !!(Port 1) >> !!(Src 102) >> !!(Dst 103), 1/6
                ;  !!(Switch 103) >> !!(Port 1) >> !!(Src 103) >> !!(Dst 101), 1/6
                ;  !!(Switch 103) >> !!(Port 1) >> !!(Src 103) >> !!(Dst 102), 1/6 ]

let read_demands (dem_file : string) (topo : Topology.t) =
  let str_node_map =
    Topology.fold_vertexes
    (fun v acc ->
      StringMap.add acc ~key:(Node.name (vertex_to_label topo v)) ~data:v
    ) topo StringMap.empty in
  In_channel.with_file dem_file
    ~f:(fun file ->
        In_channel.fold_lines file
          ~init:SrcDstMap.empty
          ~f:(fun acc line ->
            let entries = Array.of_list (String.split line ~on:' ') in
            let src = StringMap.find_exn str_node_map entries.(0) in
            let dst = StringMap.find_exn str_node_map entries.(1) in
            let dem = num_of_string entries.(2) in
            SrcDstMap.add acc ~key:(src,dst) ~data:dem
            ))

let demands_to_inp_dist_pnk demands (v_id, id_v) =
  let sum = SrcDstMap.fold demands ~init:(num_of_int 0)
    ~f:(fun ~key:_ ~data:dem acc -> dem +/ acc) in
  let dist = SrcDstMap.fold demands ~init:[]
    ~f:(fun ~key:(s,d) ~data:dem acc ->
      let src_id = VertexMap.find_exn v_id s in
      let dst_id = VertexMap.find_exn v_id d in
      (!!(Switch src_id) >> !!(Port 1) >> !!(Src src_id) >> !!(Dst dst_id), dem // sum)::acc) in
  ?@dist

let analyze_routing_scheme (routing_pol : Pol.t) (topo_pol : Pol.t) (inp_dist : Pol.t) (include_links_in_path : bool)=
  let pnk_program = if include_links_in_path then
      routing_pol
    else
      (Star (topo_pol >> routing_pol)) >> topo_pol in
  let p = (inp_dist >> pnk_program) in
  Dist.print (eval 10 p);
  expectation' 1 p ~f:path_length
    |> Prob.to_dec_string
    |> Printf.printf "Latency: %s\n%!"

let () = begin
  let include_links_in_path = true in
  if (not include_links_in_path) then failwith "Need a better way to compose local programs with topology" else
  let topo = Parse.from_dotfile "examples/3cycle.dot" in
  let vertex_id_bimap = gen_node_pnk_id_map topo in
  let topo_pol = topo_to_pnk topo vertex_id_bimap in
  let scheme_spf = route_spf topo in
  let scheme_ksp = route_ksp topo 3 in
  let routing_spf_pol = routing_scheme_to_pnk topo scheme_spf include_links_in_path vertex_id_bimap in
  let routing_ksp_pol = routing_scheme_to_pnk topo scheme_ksp include_links_in_path vertex_id_bimap in
  let inp_dist = read_demands "examples/3cycle.dem" topo in
  let inp_dist_pol = demands_to_inp_dist_pnk inp_dist vertex_id_bimap in
  Printf.printf "Topo: %s\n" (Pol.to_string topo_pol);
  Printf.printf "SPF Scheme: %s" (dump_scheme topo scheme_spf);
  Printf.printf "KSP Scheme: %s" (dump_scheme topo scheme_ksp);
  Printf.printf "SPF Routing: %s\n: " (Pol.to_string routing_spf_pol);
  Printf.printf "Input dist: %s" (Pol.to_string inp_dist_pol);
  analyze_routing_scheme routing_spf_pol topo_pol inp_dist_pol include_links_in_path;
  Printf.printf "KSP Routing: %s\n: " (Pol.to_string routing_ksp_pol);
  analyze_routing_scheme routing_ksp_pol topo_pol inp_dist_pol include_links_in_path
  end

open Core.Std
open Frenetic_Network
open Net
open Topology
open Frenetic_ProbNetKAT_Interpreter

include Interp(Hist)(PreciseProb)

open Pol

module VertexOrd = struct
  type t = Topology.vertex [@@deriving sexp]
  let compare = Pervasives.compare
end

module VertexMap = Map.Make(VertexOrd)
module IntMap = Map.Make(Int)

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

type flow_decomp = float PathMap.t
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
  PathMap.iter
    pps
    ~f:(fun ~key:path ~data:prob -> Printf.bprintf buf "[%s] @ %f\n" (dump_edges t path) prob);
  Buffer.contents buf

let dump_scheme (t:Topology.t) (s:scheme) : string =
  let buf = Buffer.create 101 in
  SrcDstMap.iter s ~f:(fun ~key:(v1,v2) ~data:pps ->
                       Printf.bprintf buf "%s -> %s :\n  %s\n"
                                      (Node.name (Net.Topology.vertex_to_label t v1))
                                      (Node.name (Net.Topology.vertex_to_label t v2))
                                      (dump_path_prob_set t pps));
  Buffer.contents buf



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

let topo_to_pnk (topo : Topology.t) (v_id, id_v) =
  (* Topology dot file to ProbNetKAT program *)
  let get_portid port =
    match Int32.to_int port with
    | None -> failwith "Invalid port"
    | Some x -> x in

  fold_edges (fun edge pol_acc ->
    let src, src_port = edge_src edge in
    let dst, dst_port = edge_dst edge in
    let src_id = VertexMap.find_exn v_id src in
    let dst_id = VertexMap.find_exn v_id dst in
    let edge_src_test = Seq (Test (Switch src_id), Test (Port (get_portid src_port))) in
    let edge_dest_mod = Seq (Mod (Switch dst_id), Mod (Port (get_portid dst_port))) in
    let edge_prog = Seq (edge_src_test, Seq(Dup, Seq(edge_dest_mod, Dup))) in
    if pol_acc = Drop then
      edge_prog
    else
      Union (edge_prog, pol_acc)
    ) topo Drop

(******************* Routing Schemes *******************)
(* Topology -> scheme *)

let route_spf (topo : Topology.t) =
  let device v = let lbl = Topology.vertex_to_label topo v in (Node.device lbl) in
  let apsp = NetPath.all_pairs_shortest_paths ~topo:topo
    ~f:(fun x y ->
      (match (device x, device y) with | (Node.Host,Node.Host) -> true | _ -> false)
      ) in
  List.fold_left apsp ~init:SrcDstMap.empty ~f:(fun acc (c,v1,v2,p) ->
    SrcDstMap.add acc ~key:(v1,v2) ~data:( PathMap.singleton p 1.0) )
(*
let path_to_pnk (topo : Topology) (path : path) =
  List.fold_left path
    ~init:Id
    ~f:(fun acc edge ->
       Seq ())
let routing_scheme_to_pnk (topo : Topology.t) (routes : scheme) (v_id, id_v) =
  SrcDstMap.fold routes
  ~init:Drop
  ~f:(fun ~key:(src,dst) ~data:path_dist pol_acc ->
    if src = dst then
      pol_acc
    else
      PathMap.fold path_dist
      ~init:pol_acc
      ~f:(fun ~key:path ~data:prob acc ->
  )
  )
*)
let () = begin
  let topo = Parse.from_dotfile "3cycle.dot" in
  let vertex_id_bimap = gen_node_pnk_id_map topo in
  let spf = route_spf topo in
  (*let routing_pol = routing_scheme_to_pnk topo spf vertex_id_bimap in*)
  Printf.printf "Topo: %s\n" (Pol.to_string (topo_to_pnk topo vertex_id_bimap));
  Printf.printf "Scheme: %s" (dump_scheme topo spf);
  (*Printf.printf "Routing: %s\n" (Pol.to_string routing_pol)*)
end

open Core.Std
open Frenetic_Network
open Frenetic_ProbNetKAT_Interpreter
open Net
open Num
open Topology

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
  PathMap.iter
    pps
    ~f:(fun ~key:path ~data:prob ->
      Printf.bprintf buf "[%s] @ %f\n" (dump_edges t path) (float_of_num prob));
  Buffer.contents buf

let dump_scheme (t:Topology.t) (s:scheme) : string =
  let buf = Buffer.create 101 in
  SrcDstMap.iter s
    ~f:(fun ~key:(v1,v2) ~data:pps ->
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
    let edge_src_test = Seq (Test (Switch src_id), Test (Port (port_to_pnk src_port))) in
    let edge_dest_mod = Seq (Mod (Switch dst_id), Mod (Port (port_to_pnk dst_port))) in
    let edge_prog = Seq (edge_src_test, Seq(Dup, Seq(edge_dest_mod, Dup))) in
    if pol_acc = Drop then
      edge_prog
    else
      Union (edge_prog, pol_acc)
    ) topo Drop

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

let path_to_pnk (topo : Topology.t) (path : path) (include_links : bool) (v_id, id_v) =
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
        Seq(Test(Switch src_sw_id),
            Seq(Test(Port src_pt_id),
               Seq(Dup,
                  Seq(Mod(Switch dst_sw_id),
                      Seq(Mod(Port dst_pt_id),
                          Dup))))) in
      let new_pol =
        if in_sw_id = 0 && in_pt_id = 0 then
          if include_links then edge_pol
          else pol
        else
          let curr_sw_pol = (* policy at switch : dst_port of prev edge to src_port of curr edge *)
            Seq(Test(Switch in_sw_id),
              Seq(Test(Port in_pt_id),
                  Seq(Mod(Switch src_sw_id),
                      Mod(Port src_pt_id)))) in
          if pol = Drop && include_links then Seq(curr_sw_pol, edge_pol)
          else if pol = Drop && (not include_links) then curr_sw_pol
          else if pol <> Drop && include_links then Seq(pol, Seq(curr_sw_pol, edge_pol))
          else Union(pol, curr_sw_pol) in
      ((dst_sw_id, dst_pt_id), new_pol)
    ) in
      pol

let routing_scheme_to_pnk (topo : Topology.t) (routes : scheme) (include_links : bool) (v_id, id_v) =
  SrcDstMap.fold routes
  ~init:Drop
  ~f:(fun ~key:(src,dst) ~data:path_dist pol_acc ->
    if src = dst then
      pol_acc
    else
      let src_id = VertexMap.find_exn v_id src in
      let dst_id = VertexMap.find_exn v_id dst in
      let path_choices = PathMap.fold path_dist
      ~init:[]
      ~f:(fun ~key:path ~data:prob acc ->
        (path_to_pnk topo path include_links (v_id, id_v), prob)::acc) in
      let sd_route_pol =
        Seq (Test(Src src_id),
            Seq (Test(Dst dst_id),
                Choice path_choices)) in
      if pol_acc = Drop then
        sd_route_pol
      else
        Union (pol_acc, sd_route_pol))

let () = begin
  let include_links_in_path = true in
  let topo = Parse.from_dotfile "examples/3cycle.dot" in
  let vertex_id_bimap = gen_node_pnk_id_map topo in
  let topo_pol = topo_to_pnk topo vertex_id_bimap in
  let spf = route_spf topo in
  let routing_pol = routing_scheme_to_pnk topo spf include_links_in_path vertex_id_bimap in
  Printf.printf "Topo: %s\n" (Pol.to_string topo_pol);
  Printf.printf "Scheme: %s" (dump_scheme topo spf);
  Printf.printf "Routing: %s\n\n" (Pol.to_string routing_pol);
  let pnk_program = if include_links_in_path then
      routing_pol
    else
      Seq(Star (Seq (topo_pol, routing_pol)), topo_pol) in
  Dist.print (eval 10 (!!(Switch 101) >> !!(Port 10) >> !!(Src 101) >> !!(Dst 102) >> pnk_program))
end

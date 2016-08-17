open Core.Std
open Frenetic_NetKAT
open Frenetic_OpenFlow

(** Utility functions and imports *)
module Compiler = Frenetic_NetKAT_Compiler
module Fabric = Frenetic_Fabric

let union = Frenetic_NetKAT_Optimize.mk_big_union
let compile_local =
  let open Compiler in
  compile_local ~options:{ default_compiler_options with cache_prepare = `Keep }


(** Modules *)

module SwitchSet = Set.Make(struct
    type t = switchId [@@deriving sexp]
    let compare = Pervasives.compare
  end)

module SwitchTable = Hashtbl.Make(struct
    type t = switchId [@@deriving sexp]
    let compare = Pervasives.compare
    let hash = Hashtbl.hash end)

(** Essential types *)
type stream = Frenetic_Fabric.stream
type restriction = switchId * pred * switchId * pred
type ffunc = flow list SwitchTable.t
type heuristic =
  | Graphical
  | Synthesis
  | Distance

(** Basic graph abstraction to support synthesis *)

module SynNode = struct
  type t = switchId * flow list

  let default = (0L, [])

  let compare l1 l2 = Pervasives.compare l1 l2
  let hash l1       = Hashtbl.hash l1

  let equal (sw,fls) (sw',fls') =
    sw = sw' && fls = fls'

  let to_string (sw,fls) =
    sprintf "Switch %Ld\n%s" sw (string_of_flowTable fls)
end

module SynEdge = struct
  open Frenetic_NetKAT

  type t = (portId * portId)

  let default = (0l, 0l)

  let compare = Pervasives.compare
  let hash    = Hashtbl.hash
  let equal (s,d) (s',d') =
    s = s' && d = d'
end

module SynGraph = struct
  module G = Graph.Persistent.Digraph.ConcreteBidirectionalLabeled
      (SynNode)
      (SynEdge)
  include G
end

type graph = SynGraph.t

let get_switches (topo:policy) =
  let rec populate topo set = match topo with
    | Union(p1, p2) ->
      populate p1 (populate p2 set)
    | Link(s1,_,s2,_) ->
      SwitchSet.add (SwitchSet.add set s1) s2
    | p -> failwith (sprintf "Unexpected construct in topology: %s\n"
                       (Frenetic_NetKAT_Pretty.string_of_policy p)) in
  populate topo SwitchSet.empty

let starts_at tbl sw (src,_,_,_) =
  (* Does sw precede (sw',pt') in the topology, using a predecessor table *)
  let precedes sw (sw',pt') =
    match Hashtbl.Poly.find tbl (sw',pt') with
    | Some (pre_sw,pre_pt) -> pre_sw = sw
    | None -> false in
  sw = (fst src) || precedes sw src

let syngraph (fabric:policy) (topo:policy) : graph =
  let fdd = compile_local fabric in
  let switches = get_switches topo in
  SwitchSet.fold switches ~init:SynGraph.empty
    ~f:(fun g sw ->
        let table = Compiler.to_table sw fdd in
        SynGraph.add_vertex g (sw,table))

let harmonize precedes succeeds (from_policy:stream list) (from_fabric:stream list) : policy * policy =
  (Filter True, Filter True)

let synthesize ?(heuristic=Graphical) (policy:policy) (fabric:policy) (topo:policy) : policy =
  match heuristic with
  | Graphical ->
    let open Frenetic_Fabric in
    let  ins, outs = retarget (streams_of_policy policy) (streams_of_policy fabric) topo in
    let ingress = union ins in
    let egress  = union outs in
    Union (ingress, egress)
  | Synthesis ->
    let predecessors = (Fabric.find_predecessors topo) in
    let precedes = Fabric.precedes predecessors in
    let succeeds = Fabric.succeeds (Fabric.find_successors topo) in
    let harmonize = harmonize precedes succeeds in

    let policy_streams = Frenetic_Fabric.streams_of_policy policy in
    let fabric_streams = Frenetic_Fabric.streams_of_policy fabric in

    (* Collect all streams by source *)
    let sourced_streams = SwitchTable.create ~size:(List.length policy_streams) () in
    List.iter policy_streams ~f:(fun ( (src,dst,conds,action) as stream ) ->
        SwitchTable.add_multi sourced_streams (fst src) stream);

    (* For each source, find the fabric streams that could be used to implement
       each stream of the policy *)
    let ins, outs = SwitchTable.fold sourced_streams ~init:([],[])
        ~f:(fun ~key:sw ~data:streams (ins, outs) ->
            let fabric_streams = List.filter fabric_streams
                ~f:(starts_at predecessors sw) in
            let ingress, egress = harmonize streams fabric_streams in
            (ingress::ins, egress::outs)) in
    Union(union ins, union outs)
  | Distance -> failwith "Distance heuristic not yet implemented"



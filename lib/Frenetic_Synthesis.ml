open Core.Std
open Frenetic_NetKAT
open Frenetic_OpenFlow

(** Utility functions and imports *)
module Compiler = Frenetic_NetKAT_Compiler
module Fabric = Frenetic_Fabric

let flatten = Frenetic_Util.flatten
let union = Frenetic_NetKAT_Optimize.mk_big_union
let seq   = Frenetic_NetKAT_Optimize.mk_big_seq
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
type place = Fabric.place
type stream = Fabric.stream
type restriction = switchId * pred * switchId * pred
type ffunc = flow list SwitchTable.t
type approach =
  | Graphical
  | Synthesis
type heuristic =
  | Random of int * int
  | MaxSpread
  | MinSpread
  | MinConflict

type perm_state = {
  streams : stream list
; options : (stream array) array
; indices : int    array }

(* TODO(basus): This needs to go into a better topology module *)
type topology = {
  topo : policy
; preds : (place, place) Hashtbl.t
; succs : (place, place) Hashtbl.t }

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

  type t = (portId *portId)

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

let syngraph (fabric:policy) (topo:policy) : graph =
  let fdd = compile_local fabric in
  let switches = get_switches topo in
  SwitchSet.fold switches ~init:SynGraph.empty
    ~f:(fun g sw ->
        let table = Compiler.to_table sw fdd in
        SynGraph.add_vertex g (sw,table))

(** Functions dealing with the permutation state *)
(* Initialize the permutation state from the pairs of streams, and candidate
   stream lists *)
let init_perm_state (pairs: (stream * stream list) list) : perm_state =
  let streams = List.map pairs ~f:fst in
  let options = Array.of_list (List.map pairs ~f:(fun s -> snd s |> Array.of_list)) in
  let indices = Array.create ~len:(List.length pairs) 0 in
  { streams; options; indices }

(* Get the next permutation of candidate streams, update state accordingly, and *)
(* provide a boolean to indicate when all permutations have been exhausted. *)
let next_perm_state (state:perm_state) : (stream list * perm_state * bool) =
  let working_set = Array.foldi state.options ~init:[] ~f:(fun i acc streams ->
      let stream = streams.(state.indices.(i)) in
      stream::acc) in

  let _ = Array.foldi state.indices ~init:true ~f:(fun i incr el ->
      if incr then begin
        let length = Array.length state.options.(i) in
        if ( el <  length - 1 ) then
          begin state.indices.(i) <- (el + 1); false end
        else if (el = length - 1) then
          begin state.indices.(i) <- 0; true end
        else
          failwith "Illegal permutation index. Something has gone very wrong."
      end else false) in

  let completed = Array.for_all state.indices ~f:(fun i -> i = 0) in
  (List.rev working_set, state, completed)

(* Pick n random elements from options *)
let rec random_picks options n =
  let rec aux options n acc =
    if n = 0 then acc
    else
      let index = Random.int (Array.length options) in
      let acc' = options.(index)::acc in
      aux options (n-1) acc' in
  aux (Array.of_list options) n []

(* A fabric stream can be used to carry a policy stream if they start and end at
   the same, or immediately adjacent locations. This is decided using the
   predecessor and successor tables. *)
let adjacent preds succs (src,dst,_,_) fab_stream =
  Fabric.Topo.starts_at preds (fst src) fab_stream &&
  Fabric.Topo.stops_at succs (fst dst) fab_stream

let go_to topology ((src_sw, src_pt) as src) ((dst_sw, dst_pt) as dst) =
  let pt = if src_sw = dst_sw then dst_pt
  else match Fabric.Topo.precedes topology.preds src dst with
    | Some pt -> pt
    | None ->
      failwith (sprintf "Cannot go to %s from %s in the given topology"
        (Fabric.string_of_place src) (Fabric.string_of_place dst)) in
  Mod (Location (Physical pt))

let come_from topology ((src_sw, src_pt) as src) ((dst_sw, dst_pt) as dst) =
  let pt = if src_sw = dst_sw then src_pt
    else match Fabric.Topo.succeeds topology.succs dst src with
      | Some pt -> pt
      | None ->
      failwith (sprintf "Cannot go to %s from %s in the given topology"
                  (Fabric.string_of_place src) (Fabric.string_of_place dst)) in
  And( Test( Switch dst_sw ),
       Test( Location( Physical pt )))

(* Given a policy stream and a fabric stream, generate edge policies to implement *)
(* the policy stream using the fabric stream *)
let to_netkat topo
    ((src,dst,cond,actions) as pol)
    ((src',dst',cond',actions') as fab)
    (tag:int): policy * policy =
  let open Fabric.Condition in
  let strip_vlan = 0xffff in
  let satisfy, restore =
    if places_only cond' then
      [ Mod( Vlan tag ) ], [ Mod( Vlan strip_vlan ) ]
    else if is_subset cond' cond then
      let satisfy = satisfy cond' in
      let restore = undo cond' cond in
      (satisfy, restore)
    else
      let mods = satisfy cond' in
      let encapsulate = Mod( Location( Pipe( "encapsulate" ))) in
      let restore = Mod( Location( Pipe( "decapsulate" ))) in
      ( encapsulate::mods , [ restore ]) in
  let to_fabric = go_to topo src src' in
  let to_edge   = Mod( Location (Physical (snd dst))) in
  let in_filter  = Filter (Fabric.pred_of_condition cond) in
  let out_filter = Filter (come_from topo dst' dst) in
  let modify = Frenetic_Fdd.Action.to_policy actions in
  let ingress = seq ( flatten [
      [in_filter]; satisfy; [to_fabric] ]) in
  let egress  = seq ( flatten [
      [out_filter]; restore; [modify]; [to_edge]]) in
  ingress, egress



(* Given a list of policy streams, and the selected fabric stream to implement *)
(* it, generate the appropriate NetKAT ingress and egress programs *)
let netkatize topo (pol:stream) (fabs:stream list) tag : policy * policy =
  let ins, outs = List.map fabs ~f:(fun stream ->
      to_netkat topo pol stream tag
    ) |> List.unzip in
  (union ins, union outs)


let matching (usable: stream -> stream -> bool) heuristic topology
    (from_policy:stream list) (from_fabric:stream list) : policy list * policy list =

  (* For each policy stream, find the set of fabric streams that could be used
     to carry it. This could be extended in the future with further conditions,
     in which case we would need more general constraints. *)
  let partitions = List.fold_left from_policy ~init:[]
      ~f:(fun acc stream ->
          let streams = List.filter from_fabric ~f:(usable stream) in
          let partition = (stream, streams) in
          (partition::acc)) in

  (* Pick a smaller set of fabric streams to actually carry the policy streams,
     based on a given heuristics. *)
  let pairs = match heuristic with
    | Random(num, seed) ->
      Random.init seed;
      List.fold_left partitions ~init:[] ~f:(fun acc (stream, opts) ->
          let picks = random_picks opts num in
          (stream, picks)::acc)
    | MaxSpread
    | MinSpread
    | MinConflict -> [] in

  (* Use the policy streams and the corresponding fabric streams to generate
     edge NetKAT programs that implement the policy streams atop the fabric
     streams. *)
  let ins, outs, _ = List.fold_left pairs ~init:([],[], 0)
      ~f:(fun (ins, outs, tag) (stream, picks) ->
          let ins', outs' = netkatize topology stream picks tag in
          (ins'::ins, outs'::outs, tag+1)) in
  ( ins, outs )


let synthesize ?(approach=Graphical) ?(heuristic=Random(1,1337))
    (policy:policy) (fabric:policy) (topo:policy) : policy =
  (* Streams are condition/modification pairs with empty actioned pairs filtered out *)
  let policy_streams = Fabric.streams_of_policy policy in
  let fabric_streams = Fabric.streams_of_policy fabric in
  let ins, outs = match approach with
  | Graphical ->
    let open Frenetic_Fabric in
    retarget policy_streams fabric_streams topo
  | Synthesis ->
    let preds = Fabric.Topo.predecessors topo in
    let succs = Fabric.Topo.successors topo in
    let usable = adjacent preds succs in
    let topology = {topo; preds; succs} in
    matching usable heuristic topology policy_streams fabric_streams in
  Union(union ins, union outs)

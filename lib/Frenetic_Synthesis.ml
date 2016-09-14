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

type heuristic =
  | Random of int * int
  | MaxSpread
  | MinSpread

type perm_state = {
  streams : stream list
; options : (stream array) array
; indices : int    array }

(* TODO(basus): This needs to go into a better topology module *)
type topology = {
  topo : policy
; preds : (place, place) Hashtbl.t
; succs : (place, place) Hashtbl.t }

type decider = topology -> stream -> stream -> bool
type generator = topology -> (stream * stream list) list -> (policy * policy)


module type MAPPING = sig
  val decide : topology -> stream -> stream -> bool
  val generate : topology -> (stream * stream list) list -> (policy * policy)
end


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
  let options = Array.of_list options in
  let bound = Array.length options in
  let rec aux n acc =
    if n = 0 then acc
    else
      let index = Random.int bound in
      let acc' = options.(index)::acc in
      aux (n-1) acc' in
  if bound = -1 then [] else aux n []

(** Topology related functions. Again, need to be replaced by a better topology module. *)
(* Check if the fabric stream and the policy stream start and end at the same,
   or immediately adjacent locations. This is decided using predecessor and
   successor tables. *)
let adjacent topo (src,dst,_,_) fab_stream =
  Fabric.Topo.starts_at topo.preds (fst src) fab_stream &&
  Fabric.Topo.stops_at  topo.succs (fst dst) fab_stream

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
  And(Test( Switch dst_sw ),
       Test( Location( Physical pt )))


module Generic:MAPPING = struct
  (** Functions for generating edge NetKAT programs from matched streams **)
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

  (* Given a list of (policy streams, possible fabric streams
     generate the appropriate NetKAT ingress and egress programs *)
  let generate topo pairs : policy * policy =
    let ins, outs, _ = List.fold_left pairs ~init:([],[], 0)
        ~f:(fun (ins, outs, tag) (stream, picks) -> match picks with
            | [] ->
              printf "No available channel for policy stream |%s|\n"
                (Fabric.string_of_stream stream);
              (ins,outs,tag)
            | picks ->
              let ins', outs' = List.fold picks ~init:(ins,outs)
                  ~f:(fun (ins, outs) fab ->
                      let ins', outs' = to_netkat topo stream fab tag in
                      (ins'::ins, outs'::outs)) in
              (ins', outs', tag+1)) in
    (union ins, union outs)

  (** A fabric stream can carry a policy stream, without encapsulation iff
      1. The endpoints of the two streams are adjacent (see the `adjacent` function)
      and
      2. The fabric stream's conditions only require incoming traffic to enter at
       a certain location only or
      3. The set of fields checked by the fabric stream's conditions are a subset
       of those checked by the policy stream. **)
  let decide topo
      ((src,dst,cond,actions) as pol)
      ((src',dst',cond',actions') as fab) =
    adjacent topo pol fab &&
    ( Fabric.Condition.places_only cond' || Fabric.Condition.is_subset cond' cond)
end

module Optical : MAPPING = struct
  let decide topo ((_,_,cond,_) as pol) ((_,_,cond',_) as fab) =
    adjacent topo pol fab && Fabric.Condition.places_only cond'

  let to_netkat topo
    ((src,dst,cond,actions)) ((src',dst',cond',actions'))
    (tag:int): policy * policy =
  let open Fabric.Condition in
  let strip_vlan = 0xffff in
  let to_fabric = go_to topo src src' in
  let to_edge   = Mod( Location (Physical (snd dst))) in
  let in_filter  = Filter (Fabric.pred_of_condition cond) in
  let out_filter = Filter (come_from topo dst' dst) in
  let modify = Frenetic_Fdd.Action.to_policy actions in
  let ingress = seq ([ in_filter; Mod( Vlan tag ); to_fabric ]) in
  let egress  = seq ([ out_filter; Mod( Vlan strip_vlan ); modify; to_edge ]) in
  ingress, egress

  (* Use the policy streams and the corresponding fabric streams to generate
     edge NetKAT programs that implement the policy streams atop the fabric
     streams. Use a unique integer tag per policy stream to keep them
     separate. This may need to be changed to be a unique tag per
     (policy stream, fabric stream) pair. *)
  let generate topo pairs : policy * policy =
    let ins, outs, _ = List.fold_left pairs ~init:([],[], 0)
        ~f:(fun (ins, outs, tag) (stream, picks) -> match picks with
            | [] ->
              printf "No available channel for policy stream |%s|\n"
                (Fabric.string_of_stream stream);
              (ins,outs,tag)
            | picks ->
              let ins', outs' = List.fold picks ~init:(ins,outs)
                  ~f:(fun (ins, outs) fab ->
                      let ins', outs' = to_netkat topo stream fab tag in
                      (ins'::ins, outs'::outs)) in
              (ins', outs', tag+1)) in
    (union ins, union outs)

end

(** Core matching function *)
let matching (usable:decider) (to_netkat:generator)
    heuristic topology
    (from_policy:stream list) (from_fabric:stream list) : policy * policy =

  (* For each policy stream, find the set of fabric streams that could be used
     to carry it. This is done using the `usable` decider function that allows
     the caller to specify what criteria are important, perhaps according to
     the properties of the fabric. *)
  let partitions = List.fold_left from_policy ~init:[]
      ~f:(fun acc stream ->
          let streams = List.filter from_fabric ~f:(usable topology stream) in
          let partition = (stream, streams) in
          (partition::acc)) in

  (* Pick a smaller set of fabric streams to actually carry the policy streams,
     based on a given heuristic. *)
  let pairs = match heuristic with
    | Random(num, seed) ->
      Random.init seed;
      List.fold_left partitions ~init:[] ~f:(fun acc (stream, opts) ->
          let picks = random_picks opts num in
          (stream, picks)::acc)
    | MaxSpread
    | MinSpread -> [] in

  to_netkat topology pairs

module Make (M:MAPPING) = struct

  let synthesize ?(heuristic=Random(1,1337))
      (policy:policy) (fabric:policy) (topo:policy) : policy =
    (* Streams are condition/modification pairs with empty actioned pairs filtered out *)
    let policy_streams = Fabric.streams_of_policy policy in
    let fabric_streams = Fabric.streams_of_policy fabric in
    let preds = Fabric.Topo.predecessors topo in
    let succs = Fabric.Topo.successors topo in
    let topology = {topo; preds; succs} in
    let ingress, egress = matching M.decide M.generate heuristic topology
        policy_streams fabric_streams in
    Union(ingress, egress)

end

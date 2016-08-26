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
type approach =
  | Graphical
  | Synthesis
type heuristic =
  | Random of int
  | MaxSpread
  | MinSpread
  | MinConflict

type perm_state = {
  streams : stream list
; options : (stream array) array
; indices : int    array }

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

let matching predecessors successors heuristic
    (from_policy:stream list) (from_fabric:stream list) : policy list * policy list =

  (* A fabric stream can be used to carry a policy stream if it starts and ends
     at the same, or immediately adjacent locations *)
  let usable (src, dst, _,_) streams = List.filter streams ~f:(fun stream' ->
        Fabric.Topo.starts_at predecessors (fst src) stream' &&
        Fabric.Topo.stops_at successors (fst dst) stream') in

  (* For each policy stream, find the set of fabric streams that could be used
     to carry it. *)
  let partitions = List.fold_left from_policy ~init:[] ~f:(fun acc stream ->
      let streams = usable stream from_fabric in
      (stream, streams)::acc) in

  let ins, outs = match heuristic with
    | Random n -> ([], [])
    | MaxSpread
    | MinSpread
    | MinConflict -> ([], []) in

  ( ins, outs )


let synthesize ?(approach=Graphical) ?(heuristic=Random 1)
    (policy:policy) (fabric:policy) (topo:policy) : policy =
  (* Streams are condition/modification pairs with empty actioned pairs filtered out *)
  let policy_streams = Fabric.streams_of_policy policy in
  let fabric_streams = Fabric.streams_of_policy fabric in
  let ins, outs = match approach with
  | Graphical ->
    let open Frenetic_Fabric in
    retarget policy_streams fabric_streams topo
  | Synthesis ->
    let predecessors = Fabric.Topo.predecessors topo in
    let successors = Fabric.Topo.successors topo in
    matching predecessors successors heuristic
      policy_streams fabric_streams in
  Union(union ins, union outs)

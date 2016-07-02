open Core.Std
open Frenetic_Network
open Net

module EdgeSet = Topology.EdgeSet
module VertexSet = Topology.VertexSet

type topology = Net.Topology.t

type edge = Net.Topology.edge [@@deriving sexp]

type path = edge list [@@deriving sexp]

let intercalate f s = function
  | [] ->
    ""
  | h::t ->
    List.fold_left t ~f:(fun acc x -> acc ^ s ^ f x) ~init:(f h)

type demand = float [@@deriving sexp]

type probability = float [@@deriving sexp]

module PathOrd = struct
  type t = path [@@deriving sexp]
  let compare = Pervasives.compare
end

module PathMap = Map.Make(PathOrd)

module IntMap = Map.Make(Int)

module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

module VertexOrd = struct
  type t = Topology.vertex [@@deriving sexp]
  let compare = Pervasives.compare
end

module VertexMap = Map.Make(VertexOrd)

module SrcDstOrd = struct
  type t = Topology.vertex * Topology.vertex [@@deriving sexp]
  let compare = Pervasives.compare
end

module SrcDstMap = Map.Make(SrcDstOrd)

module EdgeOrd = struct
  type t = edge [@@deriving sexp]
  let compare = Pervasives.compare
end

module EdgeMap = Map.Make(EdgeOrd)

(* A flow_decomp is a flow decomposed into paths. *)
type flow_decomp = probability PathMap.t

type demands = demand SrcDstMap.t

(* A routing scheme specifies a flow_decomp for each source-destination pair. *)
type scheme = flow_decomp SrcDstMap.t


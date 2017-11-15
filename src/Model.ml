(** Module for generating ProbNetKAT network model in the style (p;t)*;p.
*)
open! Core
open Syntax
module Net = Frenetic.Network.Net

module Make(Params: sig
  val sw : string
  val pt : string
  val up : int -> int -> string

  (* switch and topo terms for a particular switch *)
  val sw_pol : int -> string policy
  val topo_pol : int -> string policy

  (* probability of link failure for particular link (sw,pt) *)
  val failure : int -> int -> Prob.t

  (* the topology *)
  val topo : Net.Topology.t
end) = struct

let rec make () : string policy =
  failwith "not implemented"

and for_switch sw =
  failwith "not implemented"

and init_up_bits sw =
  failwith "not implemented"


end

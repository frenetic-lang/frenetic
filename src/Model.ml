(** Module for generating ProbNetKAT network model in the style (p;t)*;p.
    
    TODO: probably need a parameter to get partial model (where while loop
    is unrolled a final number of times).
*)
open! Core
open Syntax
module Net = Frenetic.Network.Net

module Make(Params: sig
  val sw : string
  val pt : string
  val up : int -> int -> string

  (* switch and topo terms for a particular switch *)
  val sw_pol : int -> int -> string policy
  val topo_pol : int -> string policy

  (* probability of link failure for particular link (sw,pt) *)
  val failure : int -> int -> Prob.t

  (* the topology *)
  val topo : Net.Topology.t
  val ingress : string pred
  val eggress : string pred
end) = struct

include Params

let rec make () : string policy =
  PNK.(
    filter ingress >>
    whl (neg eggress) (
      inner_hop ()
    ) >>
    final_hop ()
  )

and inner_hop () =
  failwith "not implemented"

and for_loc sw pt =
  failwith "not implemented"

and init_up_bits sw =
  failwith "not implemented"

and final_hop () =
  failwith "not implemented"


end

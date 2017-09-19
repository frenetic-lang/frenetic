(* An interpreter for Flow tables.  This simulates what goes on in an OpenFlow switch
 * for testing purposes.
*)
open Core

module Packet : sig
  val eval : 
    Frenetic_NetKAT_Semantics.packet -> 
    Frenetic_OpenFlow.flowTable -> 
    Frenetic_NetKAT_Semantics.PacketSet.t
end

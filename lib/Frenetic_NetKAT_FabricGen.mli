open Frenetic_NetKAT

module type FABRIC_GEN = sig
  (** A [fabric] is a pair of policies [(fin,fout)] used to maintain consistency
   * of a packet's virtual and physical locations during virtual compilation. *)
  type fabric = policy list * policy list

  val generate_fabric : ?log:bool
    -> ?record_paths:string
    -> vrel:pred
    -> vtopo:policy
    -> ving:pred
    -> veg:pred
    -> ptopo:policy
    -> ping:pred
    -> peg:pred
    -> fabric
end

module FabricGen : FABRIC_GEN

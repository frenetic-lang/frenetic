(** Strategy for generating fabric used by virtual compiler *)
open Syntax

(** FabricGen signature *)
module type FABRIC_GEN = sig
  (** A [fabric] is a pair of policies [(fout,fin)] used to maintain consistency
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

(** Two-Player Adversarial Strategy *)
module FabricGen : sig
  include FABRIC_GEN
  val default_ving_pol : vrel: pred -> ping: pred -> policy option
end

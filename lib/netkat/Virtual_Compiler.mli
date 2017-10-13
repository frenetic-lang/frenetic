(** Comping Virtual NetKAT Programs *)
open Core
open Syntax
open FabricGen

(** Virtual Compiler generator parameterized by Fabric Generation strategy *)
module Make(FG:FABRIC_GEN) : sig
  (** Generate a fabric for use by virtual compiler *)
  val generate_fabric : ?log:bool
    -> ?record_paths:string
    -> vrel:pred
    -> vtopo:policy
    -> ving:pred
    -> veg:pred
    -> ptopo:policy
    -> ping:pred
    -> peg:pred
    -> FG.fabric

  (** Compile virtual policy reusing precomputed fabric *)
  val compile_with_fabric :
       vtopo:policy
    -> ving_pol:policy
    -> ving:pred
    -> veg:pred
    -> ping:pred
    -> peg:pred
    -> vpol:policy
    -> FG.fabric
    -> policy

  (** Generate fabric and use it to compile virtual policy *)
  val compile : ?log:bool
    -> ?record_paths:string
    -> vrel:pred
    -> vtopo:policy
    -> ving_pol:policy
    -> ving:pred
    -> veg:pred
    -> ptopo:policy
    -> ping:pred
    -> peg:pred
    -> policy (* vpol, the virtual policy *)
    -> policy
end

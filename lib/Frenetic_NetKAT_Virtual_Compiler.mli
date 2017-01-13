open Core.Std
open Frenetic_NetKAT
open Frenetic_NetKAT_FabricGen

module Make(FG:FABRIC_GEN) : sig
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

  val compile_with_fabric : ?log:bool
    -> ?record_paths:string
    -> vtopo:policy
    -> ving_pol:policy
    -> ving:pred
    -> veg:pred
    -> ptopo:policy
    -> ping:pred
    -> peg:pred
    -> vpol:policy
    -> FG.fabric
    -> policy

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

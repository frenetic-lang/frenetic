open Core.Std
open Frenetic_NetKAT
open Frenetic_NetKAT_FabricGen

module Make(FG:FABRIC_GEN) : sig
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

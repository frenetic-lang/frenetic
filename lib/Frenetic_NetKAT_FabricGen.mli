open Frenetic_NetKAT

module type FABRIC_GEN = sig
  val generate_fabrics :
    ?log:bool ->
    ?record_paths:string ->
    pred ->
    policy ->
    pred ->
    pred ->
    policy ->
    pred ->
    pred ->
    policy list * policy list
end

module FabricGen : FABRIC_GEN

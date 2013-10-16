open NetKAT_Types
open SDN_Headers

type explicit_topo_pol =
  | Filter of pred
  | Mod of SDN_Types.field * header_val
    (* switch, port -> switch, port *)
  | Link of header_val*header_val*header_val*header_val
  | Par of explicit_topo_pol * explicit_topo_pol
  | Seq of explicit_topo_pol * explicit_topo_pol
  | Star of explicit_topo_pol

val dehop_policy : explicit_topo_pol -> NetKAT_Types.policy
val string_of_epolicy : explicit_topo_pol -> string

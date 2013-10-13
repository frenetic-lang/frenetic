module NetworkCompiler : sig
  open NetKAT_Types
  type explicit_topo_pol =
    | Filter of pred
    | Mod of SDN_Types.field * header_val
      (* switch, port -> switch, port *)
    | Link of header_val*header_val*header_val*header_val
    | Par of explicit_topo_pol * explicit_topo_pol
    | Seq of explicit_topo_pol * explicit_topo_pol
    | Star of explicit_topo_pol

  type vheader = int * int

  type ingress_pol =
    | ITest of vheader * header_val
    | IMod of vheader * header_val
    | IPar of ingress_pol * ingress_pol
    | ISeq of ingress_pol * ingress_pol
    | IPass

  type switch_pol =
    | SFilter of pred
    | STTest of vheader * header_val
    | SMod of SDN_Types.field * header_val
    | STMod of vheader * header_val
    | SPar of switch_pol * switch_pol
    | SSeq of switch_pol * switch_pol
    | SStar of switch_pol
    | SPass
    | SDrop

  type topo_pol = 
      (* (sw,pt) -> (sw',pt') *)
    | TLink of header_val * header_val * header_val * header_val
    | TPar of topo_pol * topo_pol
    | TDrop

  type restricted_pol = ingress_pol * switch_pol * topo_pol * ingress_pol

  val dehopify : explicit_topo_pol -> restricted_pol
  val simplify_spol : switch_pol -> switch_pol
  val remove_matches : switch_pol -> switch_pol

  val string_of_spolicy : switch_pol -> string
  val string_of_ipolicy : ingress_pol -> string
  val string_of_tpolicy : topo_pol -> string
  val string_of_epolicy : explicit_topo_pol -> string
end


module SwitchCompiler : sig

  (* TODO(jnf): many other modules we could expose ... *)

  module RunTime : sig 
    (* intermediate form *)
    type i 
    val compile : NetKAT_Types.policy -> i
    val decompile : i -> NetKAT_Types.policy
    val to_table : SDN_Types.fieldVal -> i -> SDN_Types.flowTable 
  end
end

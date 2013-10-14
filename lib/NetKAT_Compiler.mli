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

  type vtag = int*int

  type vheader = 
    | Field of SDN_Types.field
    | Tag of vtag

  type virtual_pol =
    | VFilter of pred
    | VTest of vtag*header_val
    | VMod of vheader * header_val
    (* switch, port -> switch, port *)
    | VLink of header_val*header_val*header_val*header_val
    | VPar of virtual_pol * virtual_pol
    | VSeq of virtual_pol * virtual_pol
    | VStar of virtual_pol
      
  type restricted_pol = virtual_pol * virtual_pol * virtual_pol * virtual_pol

  val dehopify : explicit_topo_pol -> restricted_pol
  val simplify_vpol : virtual_pol -> virtual_pol
  val remove_matches : virtual_pol -> virtual_pol

  val string_of_vpolicy : virtual_pol -> string
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

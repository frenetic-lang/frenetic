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

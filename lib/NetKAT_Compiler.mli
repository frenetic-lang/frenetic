module SwitchCompiler : sig
  type t
  val compile : NetKAT_Types.policy -> t
  val to_netkat : t -> NetKAT_Types.policy
  val to_table : SDN_Types.fieldVal -> t -> SDN_Types.flowTable 
end

module Local : sig
  type t
  val of_policy : SDN_Types.fieldVal -> Types.policy -> t
  val to_netkat : t -> Types.policy
end

module RunTime : sig 
  type i 
  val compile : SDN_Types.fieldVal -> Types.policy -> i
  val decompile : i -> Types.policy
  val to_table : i -> SDN_Types.flowTable
end

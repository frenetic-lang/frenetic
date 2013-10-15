module Local : sig
  type t
  val of_policy : NetKAT_Types.policy -> t
  val to_netkat : t -> NetKAT_Types.policy
end

module RunTime : sig 
  (* intermediate form *)
  type i 
  val compile : NetKAT_Types.policy -> i
  val decompile : i -> NetKAT_Types.policy
  val to_table : SDN_Types.fieldVal -> i -> SDN_Types.flowTable 
end

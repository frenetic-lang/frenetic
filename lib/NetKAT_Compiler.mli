module Local : sig
  type local
  val compile : NetKAT_Types.policy -> local
  val to_netkat : local -> NetKAT_Types.policy
  val local_to_table : SDN_Types.fieldVal -> local -> SDN_Types.flowTable 
end

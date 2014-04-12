open NetKAT_Types 
open SDN_Types

type t 
val of_policy : switchId -> policy -> t
val to_netkat : t -> policy
val compile : switchId -> policy -> t
val to_table : ?optimize_fall_through:bool -> t -> flowTable

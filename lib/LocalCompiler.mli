open NetKAT_Types 
open SDN_Types

type t 
val to_netkat : t -> policy
val compile : switchId -> policy -> t
val to_table : t -> flowTable

open NetKAT_Types 
open SDN_Types

type t 
val of_policy : switchId -> policy -> t 
val to_netkat : t -> policy
val compile : switchId -> policy -> t
val decompile : t -> policy
val to_table : t -> flowTable

val eval : t -> packet -> (string * packet) list * packet list

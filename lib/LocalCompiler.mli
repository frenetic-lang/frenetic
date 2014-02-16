open NetKAT_Types 
open SDN_Types

type t 
val of_policy : switchId -> policy -> t 
val to_netkat : t -> policy
val compile : switchId -> policy -> t
val decompile : t -> policy
val to_table : t -> flowTable

exception Ambiguous_pipes of string list

val from_pipe : t -> packet -> string option

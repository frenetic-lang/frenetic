open NetKAT_Types 
open SDN_Types

type t 

val compile : switchId -> policy -> t
val queries : t -> (string * pred) list

val to_policy : t -> policy
val to_table  : ?optimize_fall_through:bool -> t -> flowTable
val to_string : t -> string

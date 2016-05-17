open Core.Std
open Frenetic_NetKAT

type channel = int
type loc = switchId * portId
type hop = switchId * portId * switchId * portId
type circuit = { source : loc
               ; sink : loc
               ; path : hop list
               ; channel : channel
               }

type config = circuit list

val path_of_policies  : policy list -> ((hop list * portId), string) Result.t
val circuit_of_policy : policy -> (circuit, string) Result.t
val config_of_policy  : policy -> (config, string) Result.t
val policy_of_config  : config -> policy
val policy_of_circuit : circuit -> policy

val validate_config   : config -> (config, string) Result.t

val string_of_loc : loc -> string
val string_of_hop : hop -> string
val string_of_circuit : circuit -> string
val string_of_config : config -> string

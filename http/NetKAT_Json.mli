open Async.Std
open NetKAT_Types

val event_to_json_string : event -> string

val parse_pkt_out : Cohttp_async.Body.t ->
  (switchId * SDN_Types.pktOut) Deferred.t

val parse_update : Cohttp_async.Body.t -> policy Deferred.t
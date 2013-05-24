open NetCore_Types

val desugar : 
  (unit -> int) -> 
  (unit -> int option) -> 
  External.policy -> 
  (int, External.get_packet_handler) Hashtbl.t -> 
  Internal.pol

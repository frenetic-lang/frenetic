open NetCore_Types

val desugar : 
  (unit -> int) -> 
  (unit -> int option) -> 
  External.policy -> 
  (int, External.get_count_handler) Hashtbl.t -> 
  Internal.pol

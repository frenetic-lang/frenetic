open NetCore_Types

val desugar : (unit -> int option) 
           -> (unit -> int) 
           -> ((int, (int * External.get_count_handler * bool)) Hashtbl.t)
           -> External.policy 
           -> Internal.pol

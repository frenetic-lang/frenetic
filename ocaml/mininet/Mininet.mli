open MininetTypes

(** Parses the output of Mininet's net command. *)      
val parse_from_chan : in_channel -> string -> (node * portId * node) list

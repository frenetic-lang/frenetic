val set_log_file : bool -> string -> unit

val get_log_chan : unit -> out_channel

val printf : ('a, out_channel, unit) format -> 'a



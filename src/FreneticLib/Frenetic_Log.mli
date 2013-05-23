module Log : sig

  val set_log_file : bool -> string -> unit

  val get_log_chan : unit -> out_channel

  val printf : string -> ('a, out_channel, unit) format -> 'a

end

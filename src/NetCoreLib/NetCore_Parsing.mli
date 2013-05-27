open NetCore_Types.Internal

val parse_from_chan  : in_channel -> string -> unit Lwt.t * pol NetCore_Stream.t


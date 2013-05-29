open NetCore_Types

val parse_from_chan  : in_channel -> string -> unit Lwt.t * pol NetCore_Stream.t

val parse_literate_from_chan : in_channel -> string -> unit Lwt.t * pol NetCore_Stream.t


open NetCore_Types

val parse_from_chan  : in_channel -> string -> NetCore_SurfaceSyntax.exp

val parse_literate_from_chan : in_channel -> string ->  NetCore_SurfaceSyntax.exp

val compile_program : NetCore_SurfaceSyntax.exp -> unit Lwt.t * pol NetCore_Stream.t

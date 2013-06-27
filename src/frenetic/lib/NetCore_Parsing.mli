open NetCore_Types

val parse_by_extension : string -> NetCore_SurfaceSyntax.top

val compile_program : NetCore_SurfaceSyntax.top -> unit Lwt.t * pol NetCore_Stream.t

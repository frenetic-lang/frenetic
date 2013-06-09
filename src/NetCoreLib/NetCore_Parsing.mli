open NetCore_Types

val parse_by_extension : string -> NetCore_SurfaceSyntax.exp

val compile_program : NetCore_SurfaceSyntax.exp -> unit Lwt.t * pol NetCore_Stream.t

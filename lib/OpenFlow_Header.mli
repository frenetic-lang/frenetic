open Core.Std

type xid = Int32.t

type t = {
  version: int;
  type_code: int;
  length: int;
  xid: xid
} include Sexpable with type t := t

val size : int

val parse : Cstruct.t -> t

val marshal : Cstruct.t -> t -> unit

val to_string : t -> string

val type_code_hello : int
val type_code_error : int
val type_code_echo_request : int
val type_code_echo_reply : int

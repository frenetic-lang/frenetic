type t = {
  version: int;
  type_code: int;
  length: int;
  xid: Int32.t
}

val size : int

val parse : Cstruct.t -> t

val marshal : Cstruct.t -> t -> unit

val to_string : t -> string

open Core.Std

module type Message = sig
  type t
  include Sexpable with type t := t

  val header_of : t -> OpenFlow_Header.t

  val parse : OpenFlow_Header.t -> Cstruct.t -> t

  val marshal : t -> Cstruct.t -> int

  val to_string : t -> string
end

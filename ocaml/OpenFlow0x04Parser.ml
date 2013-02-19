open Cstruct
open OpenFlow0x04Types

module type PARSER = sig

  
  type t

  val parse : Cstruct.buf -> t

  (** [marshal t buf] writes [t] to the head of  [buf] and returns the number o
      bytes written. [marshal] throws an exception if [buf] does not have
      enough space.*)
  val marshal : t -> Cstruct.buf -> int

  (** [sizeof t] returns the number bytes [t] consumes when marshalled. *)
  val sizeof : t -> int

end

module Oxm : PARSER with type t = oxm = struct

  type t = oxm

  let parse buf = failwith "NYI"

  let marshal t buf = failwith "NYI"

  let sizeof t = failwith "NYI"
end 

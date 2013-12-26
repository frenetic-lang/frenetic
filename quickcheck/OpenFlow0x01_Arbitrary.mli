open OpenFlow0x01
open QuickCheck


(* arbitrary instance for usigned integers. Still uses the `int` type. *)
val arbitrary_uint : int arbitrary

val arbitrary_uint8 : int arbitrary
val arbitrary_uint16 : int arbitrary
val arbitrary_uint32 : Int32.t arbitrary

(** A serializable data structure suitable for roundtrip testing *)
module type OpenFlow0x01_Arbitrary = sig

    (* The serializable data type. *)
    type t
    (* The data type `t` will be serialized to. *)
    type s

    (* A QuickCheck arbitrary instance for the serializable data type *)
    val arbitrary : t arbitrary

    (** [to_string v] pretty-print [v] *)
    val to_string : t -> string

    (** [parse v] deserialize [v] to a value of type [t] *)
    val parse : s -> t
    (** [marshal v] serialize [v] to a value of type [s] *)
    val marshal : t -> s

end

(** A data structure you can serialize to a [Cstruct] *)
module type OpenFlow0x01_ArbitraryCstruct = sig
  (* The serializable data type. *)
  type t

  (* A QuickCheck arbitrary instance for the serializable data type *)
  val arbitrary : t arbitrary

  (** [to_string v] pretty-print [v] *)
  val to_string : t -> string

  (** [parse v] deserialize [v] to a value of type [t] *)
  val parse : Cstruct.t -> t
  (** [marshal v b] serialize [v] into an [s], returning the number of bytes
   * taken up in [s]. *)
  val marshal : t -> Cstruct.t -> int

  (** [size_of v] the serialized size of [v] *)
  val size_of : t -> int

end

module OpenFlow0x01_Unsize : functor (ArbS : OpenFlow0x01_ArbitraryCstruct) ->
  (OpenFlow0x01_Arbitrary with type t = ArbS.t
                           and type s = Cstruct.t)

module Wildcards : OpenFlow0x01_Arbitrary
module Match : OpenFlow0x01_ArbitraryCstruct
module PseudoPort : OpenFlow0x01_Arbitrary
module Action : OpenFlow0x01_ArbitraryCstruct
module FlowMod : sig
  include OpenFlow0x01_ArbitraryCstruct

  module Command : OpenFlow0x01_Arbitrary
  module Timeout : OpenFlow0x01_Arbitrary
end

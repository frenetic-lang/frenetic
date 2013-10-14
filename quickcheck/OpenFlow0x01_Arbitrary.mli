open OpenFlow0x01
open QuickCheck


(* arbitrary instance for usigned integers. Still uses the `int` type. *)
val arbitrary_uint : int arbitrary

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

module Wildcards : OpenFlow0x01_Arbitrary
module Match : OpenFlow0x01_Arbitrary
module PseudoPort : OpenFlow0x01_Arbitrary

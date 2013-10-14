open OpenFlow0x01
open QuickCheck


(* arbitrary instance for usigned integers. Still uses the `int` type. *)
val arbitrary_uint : int arbitrary

module type OpenFlow0x01_Arbitrary = sig
    type t
    type s

    val arbitrary : t arbitrary

    val to_string : t -> string

    val parse : s -> t
    val marshal : t -> s
end

module Wildcards : OpenFlow0x01_Arbitrary
module Match : OpenFlow0x01_Arbitrary
module PseudoPort : OpenFlow0x01_Arbitrary

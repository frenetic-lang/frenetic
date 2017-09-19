open Frenetic_OpenFlow0x01
open QuickCheck

(** A serializable data structure suitable for roundtrip testing *)
module type Frenetic_OpenFlow0x01_Arbitrary = sig

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
module type Frenetic_OpenFlow0x01_ArbitraryCstruct = sig
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

module Frenetic_OpenFlow0x01_Unsize : functor (ArbS : Frenetic_OpenFlow0x01_ArbitraryCstruct) ->
  (Frenetic_OpenFlow0x01_Arbitrary with type t = ArbS.t
                           and type s = Cstruct.t)

module Wildcards : Frenetic_OpenFlow0x01_Arbitrary
module Match : Frenetic_OpenFlow0x01_ArbitraryCstruct
module PseudoPort : Frenetic_OpenFlow0x01_Arbitrary
module Action : Frenetic_OpenFlow0x01_ArbitraryCstruct
module Timeout : Frenetic_OpenFlow0x01_Arbitrary

module FlowMod : sig
  include Frenetic_OpenFlow0x01_ArbitraryCstruct

  module Command : Frenetic_OpenFlow0x01_Arbitrary
end

module FlowRemoved : sig
  include Frenetic_OpenFlow0x01_ArbitraryCstruct

  module Reason : Frenetic_OpenFlow0x01_Arbitrary
end

module PortDescription : sig
  include Frenetic_OpenFlow0x01_ArbitraryCstruct

  module PortConfig : Frenetic_OpenFlow0x01_Arbitrary
  module PortState : Frenetic_OpenFlow0x01_Arbitrary
  module PortFeatures : Frenetic_OpenFlow0x01_Arbitrary
end

module PortStatus : Frenetic_OpenFlow0x01_ArbitraryCstruct

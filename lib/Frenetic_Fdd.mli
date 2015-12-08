open Core.Std

module Field : sig
  type t
    = Switch
      | Vlan
      | VlanPcp
      | VSwitch
      | VPort
      | EthType
      | IPProto
      | EthSrc
      | EthDst
      | IP4Src
      | IP4Dst
      | TCPSrcPort
      | TCPDstPort
      | Location
      | VFabric
  with sexp
  val auto_order : Frenetic_NetKAT.policy -> unit
  val set_order : t list -> unit
  val get_order : unit -> t list
  val all_fields : t list
  val to_string : t -> string
  val of_string : string -> t
  val compare : t -> t -> int
  val hash : t -> int
end

module Value : sig
  type t =
      Const of Int64.t
    | Mask of Int64.t * int
    | Pipe of string
    | Query of string
    (* TODO(grouptable): HACK, should only be able to fast fail on ports.
     * Put this somewhere else *)
    | FastFail of Int32.t list
    with sexp

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val to_string : t -> string
  val of_int : int -> t
  val of_int64 : int64 -> t
  val to_int_exn : t -> int
end

module Pattern : sig
  type t = Field.t * Value.t
  val compare : t -> t -> int
  val of_hv : Frenetic_NetKAT.header_val -> t
  val to_hv : t -> Frenetic_NetKAT.header_val
  val to_pred : t -> Frenetic_NetKAT.pred
  val to_sdn : t -> Frenetic_OpenFlow.Pattern.t -> Frenetic_OpenFlow.Pattern.t (* TODO(jnf): why does this modify an OpenFlow pattern? *)
end

module Action : sig
  type field_or_cont =
    | F of Field.t
    | K
  with sexp

  module Seq : sig
    include Map.S with type Key.t = field_or_cont
    val compare_mod_k : Value.t t -> Value.t t -> bool
    val equal_mod_k : Value.t t -> Value.t t -> bool
    val to_hvs : Value.t t -> (Field.t * Value.t) list
  end

  module Par : sig
    include Set.S with type Elt.t = Value.t Seq.t
    val to_hvs : t -> (Field.t * Value.t) list
  end

  type t = Par.t with sexp
  val one : t
  val zero : t
  val negate : t -> t
  val to_policy : t -> Frenetic_NetKAT.policy
  val demod : Pattern.t -> t -> t
  val to_sdn : ?group_tbl:Frenetic_GroupTable0x04.t
            -> Int64.t option -> t -> Frenetic_OpenFlow.par
  val get_queries : t -> string list
  val pipes : t -> String.Set.t
  val queries : t -> string list
  val to_string : t -> string
end

module FDK : sig
  include Frenetic_Vlr.S with type v = Field.t * Value.t and type r = Action.t
  val mk_cont : int -> t
  val conts : t -> Int.Set.t
  val map_conts : t -> f:(int -> int) -> t
end

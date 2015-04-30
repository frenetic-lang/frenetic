(* TODO(jnf): why is this exception here, in a module that defines a
   data structure? Seems misplaced... *)
exception Non_local

module Field : sig 
  type t
    = Switch
      | Vlan
      | VlanPcp
      | EthType
      | IPProto
      | EthSrc
      | EthDst
      | IP4Src
      | IP4Dst
      | TCPSrcPort
      | TCPDstPort
      | Location
  with sexp
  val auto_order : Frenetic_NetKAT.policy -> unit
  val set_order : t list -> unit
  val get_order : unit -> t list
  val all_fields : t list
  val to_string : t -> string
end

module Value : sig
  type t = 
      Const of Int64.t
    | Mask of Int64.t * int
    | Pipe of string
    | Query of string with sexp

  val of_int64 : int64 -> t
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
  module Seq : sig
    type 'a t with sexp
    val singleton : Field.t -> 'a -> 'a t
    val to_alist : 'a t -> (Field.t * 'a) list
  end
  module Par : sig
    type t with sexp
    val singleton : Value.t Seq.t -> t
    val fold : t -> init:'a -> f:('a -> Value.t Seq.t -> 'a) -> 'a
  end

  type t = Par.t with sexp
  val one : t
  val zero : t
  val negate : t -> t
  val to_policy : t -> Frenetic_NetKAT.policy
  val demod : Pattern.t -> t -> t
  val to_sdn : ?in_port:int64 -> t -> Frenetic_OpenFlow.par
  val get_queries : t -> string list
  val pipes : t -> Frenetic_Util.StringSet.t
  val queries : t -> string list
end

module T : Frenetic_Vlr.S 
  with type v = Pattern.t and type r = Action.t

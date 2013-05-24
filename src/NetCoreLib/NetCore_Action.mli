open OpenFlow0x01
open Packet
open NetCore_Types.Internal

val concat_map : ('a -> 'b list) -> 'a list -> 'b list

val filter_map : ('a -> 'b option) -> 'a list -> 'b list

module type ACTION = 
 sig 
  type t 
  
  type e 
  
  val atoms : t -> e list

  val to_action : e -> t
  
  val drop : t
  
  val pass : t
  
  val apply_action : t -> lp -> lp list
  
  val par_action : t -> t -> t
  
  val seq_action : t -> t -> t
  
  val restrict_range : e -> ptrn -> ptrn
  
  val domain : e -> ptrn

  val to_string : t -> string

  val is_equal : t -> t -> bool

 end

module Output : sig
  include ACTION
    with type e = action_atom
    and type t = action
  val forward : portId -> t
  val to_all : t
  val updateDlSrc : Int64.t -> Int64.t -> t
  val updateDlDst : Int64.t -> Int64.t -> t
  val updateDlVlan : int option -> int option -> t
  val updateSrcIP : Int32.t -> Int32.t -> t
  val updateDstIP : Int32.t -> Int32.t -> t
  val updateSrcPort : int -> int -> t
  val updateDstPort : int -> int -> t
  val bucket : int -> t
  val controller : (OpenFlow0x01.switchId -> port -> packet -> action) -> t
  (* val controller : (switchId portId -> packet -> t) -> t *)
  val as_actionSequence : portId option -> t -> OpenFlow0x01.Action.sequence
end

module Bool : ACTION 
  with type t = bool

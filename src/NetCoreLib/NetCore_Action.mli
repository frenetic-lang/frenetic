open OpenFlow0x01
open Packet
open NetCore_Pattern

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
  
  val apply_atom : e -> (port * packet) -> (port * packet) option
  
  val apply_action : t -> (port * packet) -> (port * packet) list
  
  val par_action : t -> t -> t
  
  val seq_action : t -> t -> t
  
  val restrict_range : e -> NetCore_Pattern.t -> NetCore_Pattern.t
  
  val domain : e -> NetCore_Pattern.t

  val to_string : t -> string

  val is_equal : t -> t -> bool

 end

module Output : sig
  include ACTION
  val forward : portId -> t
  val to_all : t
  val updateDlSrc : Int64.t -> Int64.t -> t
  val updateDlDst : Int64.t -> Int64.t -> t
  val updateDlVlan : int option -> int option -> t
  val updateSrcIP : Int32.t -> Int32.t -> t
  val updateDstIP : Int32.t -> Int32.t -> t
  val updateSrcPort : int -> int -> t
  val updateDstPort : int -> int -> t
  val bucket : int -> bool -> t
  val as_actionSequence : portId option -> t -> OpenFlow0x01.Action.sequence
end

module Bool : ACTION 
  with type t = bool

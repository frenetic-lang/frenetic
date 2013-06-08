open OpenFlow0x01
open Packet
open NetCore_Types

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
  
  val sequence_range : e -> ptrn -> ptrn
  
  val domain : e -> ptrn

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
  val query : float -> get_count_handler -> t
  val controller : (OpenFlow0x01.switchId -> port -> packet -> action) -> t
  val apply_controller : action -> lp -> action
  val switch_part  : action -> action
  val as_actionSequence : portId option -> t -> OpenFlow0x01.Action.sequence
  val queries : t -> t
  val is_equal : t -> t -> bool
  val atom_is_equal : e -> e -> bool
  val make_transformer : (port * packet) -> (port * packet) -> action
end

module Bool : ACTION 
  with type t = bool

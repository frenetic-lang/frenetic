open OpenFlow0x01.Types 
open Packet
open Pattern

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
  
  val restrict_range : e -> Pattern.t -> Pattern.t
  
  val domain : e -> Pattern.t

  val to_string : t -> string

 end

module Output : sig
  include ACTION
  val forward : portId -> t
  val to_all : t
  val updateDlSrc : Int64.t -> Int64.t -> t
  val updateDlDst : Int64.t -> Int64.t -> t
  val updateDlVlan : int option -> int option -> t
  val bucket : int -> t
  val as_actionSequence : portId option -> t -> actionSequence
end

module Bool : ACTION 
  with type t = bool

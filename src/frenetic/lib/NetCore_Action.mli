open Packet
open NetCore_Types

module type OPENFLOW =
 sig
   module Action :
   sig
     type sequence
   end
   type switchId
   type features
   type switchEvent =
     | SwitchUp of switchId * features
     | SwitchDown of switchId
 end
       
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

  val atom_is_equal : e -> e -> bool

 end

(* module type MAKE_COMPILER_ACTION = functor (OpenFlow : OPENFLOW) -> *)
(*   sig *)
(*     include ACTION *)
(*       with type e = action_atom *)
(*     val from_nc_action : action -> t *)
(*     val as_actionSequence : portId option -> t -> OpenFlow.Action.sequence *)
(*     val queries : t -> e list *)
(*   end *)

module type COMPILER_ACTION0x01 =
  sig
    include ACTION
      with type e = action_atom
    val from_nc_action : action -> t
    (* val as_actionSequence : portId option -> t -> OpenFlow0x01.Action.sequence *)
    val queries : t -> e list
  end

module Output : 
sig
  include ACTION
      with type e = action_atom
      and type t = action
  val string_of_action : t -> string
  val from_nc_action : action -> t
  val queries : t -> e list
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
  val controller : (switchId -> port -> packet -> action) -> t
  (* val apply_controller : action -> lp -> action *)
  val switch_part  : action -> action
  val make_transformer : value -> value -> action
end


module Bool : ACTION 
  with type t = bool

module Group : 
sig
  include ACTION
      with type e = action_atom
      and type t = action list
  val string_of_action : t -> string
  val from_nc_action : action -> t
  val queries : t -> e list
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
  val controller : (switchId -> port -> packet -> action) -> t
  (* val apply_controller : action -> lp -> action *)
  val switch_part  : action -> action
  val make_transformer : value -> value -> t
end

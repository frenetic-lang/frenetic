open Packet
open OpenFlow0x01
open NetCore_Action
open NetCore_Pattern

module type CLASSIFIER = 
 sig 
  type action
  
  type t = (NetCore_Types.Internal.ptrn * action) list
  
  val scan : t -> NetCore_Types.Internal.port -> packet -> action
  
  val union : t -> t -> t
  
  val sequence : t -> t -> t
  
  val par_actions : action list -> action

  val to_string : t -> string
 end

module type MAKE  = functor (Action : ACTION) -> 
  sig include CLASSIFIER end
  with type action = Action.t

module Make : MAKE

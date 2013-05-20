open Action
open Packet
open OpenFlow0x01.Types
open Pattern

module type CLASSIFIER = 
 sig 
  type action
  
  type t = (Pattern.t * action) list
  
  val scan : t -> Pattern.port -> packet -> action
  
  val inter : t -> t -> t
  
  val union : t -> t -> t
  
  val sequence : t -> t -> t
  
  val par_actions : action list -> action

  val to_string : t -> string
 end

module type MAKE  = functor (Action : ACTION) -> 
  sig include CLASSIFIER end
  with type action = Action.t

module Make : MAKE

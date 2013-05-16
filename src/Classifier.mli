open NetworkPacket
open OpenFlow0x01Types
open Pattern

module type ACTION = 
 sig 
  type t 
  
  type e 
  
  val atoms : t -> e list
  
  val drop : t
  
  val pass : t
  
  val apply_atom : e -> (port * packet) -> (port * packet) option
  
  val apply_action : t -> (port * packet) -> (port * packet) list
  
  val par_action : t -> t -> t
  
  val seq_action : t -> t -> t
  
  val restrict_range : e -> Pattern.t -> Pattern.t
  
  val domain : e -> Pattern.t
 end

module type CLASSIFIER = 
 sig 
  module Action : ACTION
  
  type t = (Pattern.t * Action.t) list
  
  val scan : t -> Pattern.port -> packet -> Action.t
  
  val inter : t -> t -> t
  
  val union : t -> t -> t
  
  val sequence : t -> t -> t
  
  val par_actions : Action.t list -> Action.t
 end

module type MAKE  = functor (Action : ACTION) -> 
  sig include CLASSIFIER end
  with module Action = Action

module Make : MAKE

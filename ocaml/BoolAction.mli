open NetworkPacket
open OpenFlow0x01Types
open Pattern

module Make : 
 functor (Pattern_:PATTERN) ->
 sig 
  module Pattern : PATTERN
    with type port = Pattern_.port
    and type t = Pattern_.t
  
  type pattern = Pattern.t
  
  type port = Pattern.port
  
  type t = bool
  
  type e = bool
  
  val atoms : t -> e list
  
  val drop : t
  
  val pass : t
  
  val apply_atom : e -> (port * packet) -> (port * packet) option
  
  val apply_action : t -> (port * packet) -> (port * packet) list
  
  val par_action : t -> t -> t
  
  val seq_action : t -> t -> t
  
  val restrict_range : e -> pattern -> pattern
  
  val domain : e -> Pattern.t
 end


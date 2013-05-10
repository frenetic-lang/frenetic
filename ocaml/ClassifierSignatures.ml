open NetworkPacket
open PatternSignatures

module type ACTION = 
 sig 
  module Pattern : 
   PATTERN
  
  type pattern = Pattern.t
  
  type port = Pattern.port
  
  type t 
  
  type e 
  
  val atoms : t -> e list
  
  val drop : t
  
  val pass : t
  
  val apply_atom : e -> (port * packet) -> (port * packet) option
  
  val apply_action : t -> (port * packet) -> (port * packet) list
  
  val par_action : t -> t -> t
  
  val seq_action : t -> t -> t
  
  val restrict_range : e -> pattern -> pattern
  
  val domain : e -> pattern
 end

module type ACTION_SPEC = 
 sig 
  module PatternSpec : 
   PATTERN_SPEC
  
  module Action : 
   ACTION with module Pattern = PatternSpec.Pattern
 end

module type CLASSIFIER = 
 sig 
  module Action : 
   ACTION
  
  type pattern = Action.pattern
  
  type port = Action.port
  
  type action = Action.t
  
  type t = (pattern * action) list
  
  val scan : t -> port -> packet -> action
  
  val inter : t -> t -> t
  
  val union : t -> t -> t
  
  val sequence : t -> t -> t
  
  val par_actions : action list -> Action.t
 end

module type CLASSIFIER_SPEC = 
 sig 
  module Classifier : 
   CLASSIFIER
 end

module type MAKE = 
 functor (Action_:ACTION) ->
 sig 
  module Action : 
   ACTION
  
  type pattern = Action.pattern
  
  type port = Action.port
  
  type action = Action.t
  
  type t = (pattern * action) list
  
  val scan : t -> port -> packet -> action
  
  val inter : t -> t -> t
  
  val union : t -> t -> t
  
  val sequence : t -> t -> t
  
  val par_actions : action list -> Action.t
 end


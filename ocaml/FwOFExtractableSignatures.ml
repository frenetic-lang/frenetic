module type EXTRACTABLE_ATOMS = 
 sig 
  type packet 
  
  type switchId 
  
  type portId 
  
  type flowTable 
  
  type flowMod 
  
  type fromController =
  | PacketOut of portId * packet
  | BarrierRequest of int
  | FlowMod of flowMod
  
  val fromController_rect :
    (portId -> packet -> 'a1) -> (int -> 'a1) -> (flowMod -> 'a1) ->
    fromController -> 'a1
  
  val fromController_rec :
    (portId -> packet -> 'a1) -> (int -> 'a1) -> (flowMod -> 'a1) ->
    fromController -> 'a1
  
  type fromSwitch =
  | PacketIn of portId * packet
  | BarrierReply of int
  
  val fromSwitch_rect :
    (portId -> packet -> 'a1) -> (int -> 'a1) -> fromSwitch -> 'a1
  
  val fromSwitch_rec :
    (portId -> packet -> 'a1) -> (int -> 'a1) -> fromSwitch -> 'a1
  
  val abst_func : switchId -> portId -> packet -> (portId * packet) list
 end

module type EXTRACTABLE_CONTROLLER = 
 sig 
  module Atoms : 
   EXTRACTABLE_ATOMS
  
  type controller 
  
  val send :
    controller -> ((controller * Atoms.switchId) * Atoms.fromController)
    option
  
  val recv : controller -> Atoms.switchId -> Atoms.fromSwitch -> controller
 end


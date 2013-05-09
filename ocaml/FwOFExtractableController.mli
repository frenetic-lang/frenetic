open Datatypes
open FwOFExtractableSignatures
open List0
open NetCoreEval
open NetworkPacket
open OpenFlow0x01Types

module type POLICY = 
 sig 
  val abst_func :
    switchId -> portId -> (packet * bufferId) ->
    (portId * (packet * bufferId)) list
 end

module MakeAtoms : 
 functor (Policy:POLICY) ->
 sig 
  type switchId = OpenFlow0x01Types.switchId
  
  type portId = NetworkPacket.portId
  
  type packet = NetworkPacket.packet * bufferId
  
  type flowTable = ((int * Pattern.pattern) * act) list
  
  type fm =
  | AddFlow of int * Pattern.pattern * act
  
  val fm_rect : (int -> Pattern.pattern -> act -> 'a1) -> fm -> 'a1
  
  val fm_rec : (int -> Pattern.pattern -> act -> 'a1) -> fm -> 'a1
  
  type flowMod = fm
  
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
  
  val abst_func :
    OpenFlow0x01Types.switchId -> NetworkPacket.portId ->
    (NetworkPacket.packet * bufferId) ->
    (NetworkPacket.portId * (NetworkPacket.packet * bufferId)) list
 end

module MakeController : 
 functor (Atoms_:EXTRACTABLE_ATOMS) ->
 sig 
  type switchState = { theSwId : Atoms_.switchId;
                       pendingCtrlMsgs : Atoms_.fromController list }
  
  val switchState_rect :
    (Atoms_.switchId -> Atoms_.fromController list -> 'a1) -> switchState ->
    'a1
  
  val switchState_rec :
    (Atoms_.switchId -> Atoms_.fromController list -> 'a1) -> switchState ->
    'a1
  
  val theSwId : switchState -> Atoms_.switchId
  
  val pendingCtrlMsgs : switchState -> Atoms_.fromController list
  
  type srcDst = { pkSw : Atoms_.switchId; srcPt : Atoms_.portId;
                  srcPk : Atoms_.packet; dstPt : Atoms_.portId;
                  dstPk : Atoms_.packet }
  
  val srcDst_rect :
    (Atoms_.switchId -> Atoms_.portId -> Atoms_.packet -> Atoms_.portId ->
    Atoms_.packet -> 'a1) -> srcDst -> 'a1
  
  val srcDst_rec :
    (Atoms_.switchId -> Atoms_.portId -> Atoms_.packet -> Atoms_.portId ->
    Atoms_.packet -> 'a1) -> srcDst -> 'a1
  
  val pkSw : srcDst -> Atoms_.switchId
  
  val srcPt : srcDst -> Atoms_.portId
  
  val srcPk : srcDst -> Atoms_.packet
  
  val dstPt : srcDst -> Atoms_.portId
  
  val dstPk : srcDst -> Atoms_.packet
  
  type state = { pktsToSend : srcDst list; switchStates : switchState list }
  
  val state_rect : (srcDst list -> switchState list -> 'a1) -> state -> 'a1
  
  val state_rec : (srcDst list -> switchState list -> 'a1) -> state -> 'a1
  
  val pktsToSend : state -> srcDst list
  
  val switchStates : state -> switchState list
  
  val mkPktOuts_body :
    Atoms_.switchId -> Atoms_.portId -> Atoms_.packet ->
    (Atoms_.portId * Atoms_.packet) -> srcDst
  
  val mkPktOuts :
    Atoms_.switchId -> Atoms_.portId -> Atoms_.packet -> srcDst list
  
  type controller = state
  
  val send_queued :
    switchState list -> ((switchState
    list * Atoms_.switchId) * Atoms_.fromController) option
  
  val send :
    state -> ((state * Atoms_.switchId) * Atoms_.fromController) option
  
  val recv : state -> Atoms_.switchId -> Atoms_.fromSwitch -> state
  
  module Atoms : 
   sig 
    type packet = Atoms_.packet
    
    type switchId = Atoms_.switchId
    
    type portId = Atoms_.portId
    
    type flowTable = Atoms_.flowTable
    
    type flowMod = Atoms_.flowMod
    
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
 end


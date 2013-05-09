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

module MakeAtoms = 
 functor (Policy:POLICY) ->
 struct 
  type switchId = OpenFlow0x01Types.switchId
  
  type portId = NetworkPacket.portId
  
  type packet = NetworkPacket.packet * bufferId
  
  type flowTable = ((int * Pattern.pattern) * act) list
  
  type fm =
  | AddFlow of int * Pattern.pattern * act
  
  (** val fm_rect : (int -> Pattern.pattern -> act -> 'a1) -> fm -> 'a1 **)
  
  let fm_rect f = function
  | AddFlow (x, x0, x1) -> f x x0 x1
  
  (** val fm_rec : (int -> Pattern.pattern -> act -> 'a1) -> fm -> 'a1 **)
  
  let fm_rec f = function
  | AddFlow (x, x0, x1) -> f x x0 x1
  
  type flowMod = fm
  
  type fromController =
  | PacketOut of portId * packet
  | BarrierRequest of int
  | FlowMod of flowMod
  
  (** val fromController_rect :
      (portId -> packet -> 'a1) -> (int -> 'a1) -> (flowMod -> 'a1) ->
      fromController -> 'a1 **)
  
  let fromController_rect f f0 f1 = function
  | PacketOut (x, x0) -> f x x0
  | BarrierRequest x -> f0 x
  | FlowMod x -> f1 x
  
  (** val fromController_rec :
      (portId -> packet -> 'a1) -> (int -> 'a1) -> (flowMod -> 'a1) ->
      fromController -> 'a1 **)
  
  let fromController_rec f f0 f1 = function
  | PacketOut (x, x0) -> f x x0
  | BarrierRequest x -> f0 x
  | FlowMod x -> f1 x
  
  type fromSwitch =
  | PacketIn of portId * packet
  | BarrierReply of int
  
  (** val fromSwitch_rect :
      (portId -> packet -> 'a1) -> (int -> 'a1) -> fromSwitch -> 'a1 **)
  
  let fromSwitch_rect f f0 = function
  | PacketIn (x, x0) -> f x x0
  | BarrierReply x -> f0 x
  
  (** val fromSwitch_rec :
      (portId -> packet -> 'a1) -> (int -> 'a1) -> fromSwitch -> 'a1 **)
  
  let fromSwitch_rec f f0 = function
  | PacketIn (x, x0) -> f x x0
  | BarrierReply x -> f0 x
  
  (** val abst_func :
      OpenFlow0x01Types.switchId -> NetworkPacket.portId ->
      (NetworkPacket.packet * bufferId) ->
      (NetworkPacket.portId * (NetworkPacket.packet * bufferId)) list **)
  
  let abst_func =
    Policy.abst_func
 end

module MakeController = 
 functor (Atoms_:EXTRACTABLE_ATOMS) ->
 struct 
  type switchState = { theSwId : Atoms_.switchId;
                       pendingCtrlMsgs : Atoms_.fromController list }
  
  (** val switchState_rect :
      (Atoms_.switchId -> Atoms_.fromController list -> 'a1) -> switchState
      -> 'a1 **)
  
  let switchState_rect f s =
    let { theSwId = x; pendingCtrlMsgs = x0 } = s in f x x0
  
  (** val switchState_rec :
      (Atoms_.switchId -> Atoms_.fromController list -> 'a1) -> switchState
      -> 'a1 **)
  
  let switchState_rec f s =
    let { theSwId = x; pendingCtrlMsgs = x0 } = s in f x x0
  
  (** val theSwId : switchState -> Atoms_.switchId **)
  
  let theSwId s =
    s.theSwId
  
  (** val pendingCtrlMsgs : switchState -> Atoms_.fromController list **)
  
  let pendingCtrlMsgs s =
    s.pendingCtrlMsgs
  
  type srcDst = { pkSw : Atoms_.switchId; srcPt : Atoms_.portId;
                  srcPk : Atoms_.packet; dstPt : Atoms_.portId;
                  dstPk : Atoms_.packet }
  
  (** val srcDst_rect :
      (Atoms_.switchId -> Atoms_.portId -> Atoms_.packet -> Atoms_.portId ->
      Atoms_.packet -> 'a1) -> srcDst -> 'a1 **)
  
  let srcDst_rect f s =
    let { pkSw = x; srcPt = x0; srcPk = x1; dstPt = x2; dstPk = x3 } = s in
    f x x0 x1 x2 x3
  
  (** val srcDst_rec :
      (Atoms_.switchId -> Atoms_.portId -> Atoms_.packet -> Atoms_.portId ->
      Atoms_.packet -> 'a1) -> srcDst -> 'a1 **)
  
  let srcDst_rec f s =
    let { pkSw = x; srcPt = x0; srcPk = x1; dstPt = x2; dstPk = x3 } = s in
    f x x0 x1 x2 x3
  
  (** val pkSw : srcDst -> Atoms_.switchId **)
  
  let pkSw s =
    s.pkSw
  
  (** val srcPt : srcDst -> Atoms_.portId **)
  
  let srcPt s =
    s.srcPt
  
  (** val srcPk : srcDst -> Atoms_.packet **)
  
  let srcPk s =
    s.srcPk
  
  (** val dstPt : srcDst -> Atoms_.portId **)
  
  let dstPt s =
    s.dstPt
  
  (** val dstPk : srcDst -> Atoms_.packet **)
  
  let dstPk s =
    s.dstPk
  
  type state = { pktsToSend : srcDst list; switchStates : switchState list }
  
  (** val state_rect :
      (srcDst list -> switchState list -> 'a1) -> state -> 'a1 **)
  
  let state_rect f s =
    let { pktsToSend = x; switchStates = x0 } = s in f x x0
  
  (** val state_rec :
      (srcDst list -> switchState list -> 'a1) -> state -> 'a1 **)
  
  let state_rec f s =
    let { pktsToSend = x; switchStates = x0 } = s in f x x0
  
  (** val pktsToSend : state -> srcDst list **)
  
  let pktsToSend s =
    s.pktsToSend
  
  (** val switchStates : state -> switchState list **)
  
  let switchStates s =
    s.switchStates
  
  (** val mkPktOuts_body :
      Atoms_.switchId -> Atoms_.portId -> Atoms_.packet ->
      (Atoms_.portId * Atoms_.packet) -> srcDst **)
  
  let mkPktOuts_body sw srcPt0 srcPk0 = function
  | (dstPt0, dstPk0) ->
    { pkSw = sw; srcPt = srcPt0; srcPk = srcPk0; dstPt = dstPt0; dstPk =
      dstPk0 }
  
  (** val mkPktOuts :
      Atoms_.switchId -> Atoms_.portId -> Atoms_.packet -> srcDst list **)
  
  let mkPktOuts sw srcPt0 srcPk0 =
    map (mkPktOuts_body sw srcPt0 srcPk0) (Atoms_.abst_func sw srcPt0 srcPk0)
  
  type controller = state
  
  (** val send_queued :
      switchState list -> ((switchState
      list * Atoms_.switchId) * Atoms_.fromController) option **)
  
  let rec send_queued = function
  | [] -> None
  | s :: ss ->
    let { theSwId = sw; pendingCtrlMsgs = pendingCtrlMsgs0 } = s in
    (match pendingCtrlMsgs0 with
     | [] ->
       (match send_queued ss with
        | Some p ->
          let (p0, msg) = p in
          let (ss', sw') = p0 in
          Some ((({ theSwId = sw; pendingCtrlMsgs = [] } :: ss'), sw'), msg)
        | None -> None)
     | msg :: msgs ->
       Some ((({ theSwId = sw; pendingCtrlMsgs = msgs } :: ss), sw), msg))
  
  (** val send :
      state -> ((state * Atoms_.switchId) * Atoms_.fromController) option **)
  
  let rec send st =
    let { pktsToSend = pktsToSend0; switchStates = ss } = st in
    (match pktsToSend0 with
     | [] ->
       (match send_queued ss with
        | Some p ->
          let (p0, msg) = p in
          let (ss', sw) = p0 in
          Some (({ pktsToSend = []; switchStates = ss' }, sw), msg)
        | None -> None)
     | s :: pks ->
       let { pkSw = sw; srcPt = srcPt0; srcPk = srcPk0; dstPt = pt; dstPk =
         pk } = s
       in
       Some (({ pktsToSend = pks; switchStates = ss }, sw), (Atoms_.PacketOut
       (pt, pk))))
  
  (** val recv : state -> Atoms_.switchId -> Atoms_.fromSwitch -> state **)
  
  let rec recv st sw = function
  | Atoms_.PacketIn (pt, pk) ->
    let { pktsToSend = pktOuts; switchStates = ss } = st in
    { pktsToSend = (app (mkPktOuts sw pt pk) pktOuts); switchStates = ss }
  | Atoms_.BarrierReply n -> st
  
  module Atoms = Atoms_
 end


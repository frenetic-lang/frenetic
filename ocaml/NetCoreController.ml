open Classifier
open ControllerInterface
open Datatypes
open List0
open NetCoreCompiler
open NetCoreEval
open NetworkPacket
open OpenFlow0x01Types
open WordInterface

(** val prio_rec :
    Word16.t -> 'a1 coq_Classifier -> ((Word16.t * Pattern.pattern) * 'a1)
    list **)

let rec prio_rec prio = function
| [] -> []
| p :: rest ->
  let (pat, act0) = p in
  ((prio, pat), act0) :: (prio_rec (Word16.pred prio) rest)

(** val prioritize :
    'a1 coq_Classifier -> ((Word16.t * Pattern.pattern) * 'a1) list **)

let prioritize lst =
  prio_rec Word16.max_value lst

(** val packetIn_to_in : switchId -> packetIn -> input **)

let packetIn_to_in sw pktIn =
  InPkt (sw, pktIn.packetInPort, pktIn.packetInPacket,
    pktIn.packetInBufferId)

(** val maybe_openflow0x01_modification :
    'a1 option -> ('a1 -> action) -> actionSequence **)

let maybe_openflow0x01_modification newVal mkModify =
  match newVal with
  | Some v -> (mkModify v) :: []
  | None -> []

(** val modification_to_openflow0x01 : modification -> actionSequence **)

let modification_to_openflow0x01 mods =
  let { modifyDlSrc = dlSrc; modifyDlDst = dlDst; modifyDlVlan = dlVlan;
    modifyDlVlanPcp = dlVlanPcp; modifyNwSrc = nwSrc; modifyNwDst = nwDst;
    modifyNwTos = nwTos; modifyTpSrc = tpSrc; modifyTpDst = tpDst } = mods
  in
  app (maybe_openflow0x01_modification dlSrc (fun x -> SetDlSrc x))
    (app (maybe_openflow0x01_modification dlDst (fun x -> SetDlDst x))
      (app
        (maybe_openflow0x01_modification (withVlanNone dlVlan) (fun x ->
          SetDlVlan x))
        (app
          (maybe_openflow0x01_modification dlVlanPcp (fun x -> SetDlVlanPcp
            x))
          (app (maybe_openflow0x01_modification nwSrc (fun x -> SetNwSrc x))
            (app
              (maybe_openflow0x01_modification nwDst (fun x -> SetNwDst x))
              (app
                (maybe_openflow0x01_modification nwTos (fun x -> SetNwTos x))
                (app
                  (maybe_openflow0x01_modification tpSrc (fun x -> SetTpSrc
                    x))
                  (maybe_openflow0x01_modification tpDst (fun x -> SetTpDst
                    x)))))))))

(** val translate_action : portId option -> act -> actionSequence **)

let translate_action in_port act0 =
  let { modifications = mods; toPorts = ports; queries = queries0 } = act0 in
  app (modification_to_openflow0x01 mods)
    (map (fun pp ->
      match pp with
      | PhysicalPort pt ->
        (match in_port with
         | Some pt' ->
           if Word16.eq_dec pt' pt
           then Output InPort
           else Output (PhysicalPort pt)
         | None -> Output (PhysicalPort pt))
      | _ -> Output pp) ports)

(** val to_flow_mod : priority -> Pattern.pattern -> act -> flowMod **)

let to_flow_mod prio pat act0 =
  let ofMatch = Pattern.Pattern.to_match pat in
  { mfModCmd = AddFlow; mfMatch = ofMatch; mfPriority = prio; mfActions =
  (translate_action ofMatch.matchInPort act0); mfCookie = Word64.zero;
  mfIdleTimeOut = Permanent; mfHardTimeOut = Permanent; mfNotifyWhenRemoved =
  false; mfApplyToPacket = None; mfOutPort = None; mfCheckOverlap = false }

(** val flow_mods_of_classifier : act coq_Classifier -> flowMod list **)

let flow_mods_of_classifier lst =
  fold_right (fun ppa lst0 ->
    let (p, act0) = ppa in
    let (prio, pat) = p in
    if Pattern.Pattern.is_empty pat
    then lst0
    else (to_flow_mod prio pat act0) :: lst0) [] (prioritize lst)

(** val delete_all_flows : flowMod **)

let delete_all_flows =
  { mfModCmd = DeleteFlow; mfMatch =
    (Pattern.Pattern.to_match Pattern.Pattern.all); mfPriority = Word16.zero;
    mfActions = []; mfCookie = Word64.zero; mfIdleTimeOut = Permanent;
    mfHardTimeOut = Permanent; mfNotifyWhenRemoved = false; mfApplyToPacket =
    None; mfOutPort = None; mfCheckOverlap = false }

type ncstate = { policy : pol; switches : switchId list }

(** val ncstate_rect : (pol -> switchId list -> 'a1) -> ncstate -> 'a1 **)

let ncstate_rect f n =
  let { policy = x; switches = x0 } = n in f x x0

(** val ncstate_rec : (pol -> switchId list -> 'a1) -> ncstate -> 'a1 **)

let ncstate_rec f n =
  let { policy = x; switches = x0 } = n in f x x0

(** val policy : ncstate -> pol **)

let policy x = x.policy

(** val switches : ncstate -> switchId list **)

let switches x = x.switches

module type NETCORE_MONAD = 
 sig 
  type 'x m 
  
  val bind : 'a1 m -> ('a1 -> 'a2 m) -> 'a2 m
  
  val ret : 'a1 -> 'a1 m
  
  type state = ncstate
  
  val get : state m
  
  val put : state -> unit m
  
  val send : switchId -> xid -> message -> unit m
  
  val recv : event m
  
  val forever : unit m -> unit m
  
  val handle_get_packet : id -> switchId -> portId -> packet -> unit m
 end

module Make = 
 functor (Monad:NETCORE_MONAD) ->
 struct 
  (** val sequence : unit Monad.m list -> unit Monad.m **)
  
  let rec sequence = function
  | [] -> Monad.ret ()
  | cmd :: lst' -> Monad.bind cmd (fun x -> sequence lst')
  
  (** val config_commands : pol -> switchId -> unit Monad.m **)
  
  let config_commands pol0 swId =
    sequence
      (map (fun fm -> Monad.send swId Word32.zero (FlowModMsg fm))
        (delete_all_flows :: (flow_mods_of_classifier
                               (compile_opt pol0 swId))))
  
  (** val set_policy : pol -> unit Monad.m **)
  
  let set_policy pol0 =
    Monad.bind Monad.get (fun st ->
      let switch_list = st.switches in
      Monad.bind (Monad.put { policy = pol0; switches = switch_list })
        (fun x ->
        Monad.bind (sequence (map (config_commands pol0) switch_list))
          (fun x0 -> Monad.ret ())))
  
  (** val handle_switch_disconnected : switchId -> unit Monad.m **)
  
  let handle_switch_disconnected swId =
    Monad.bind Monad.get (fun st ->
      let switch_list =
        filter (fun swId' ->
          if Word64.eq_dec swId swId' then false else true) st.switches
      in
      Monad.bind (Monad.put { policy = st.policy; switches = switch_list })
        (fun x -> Monad.ret ()))
  
  (** val handle_switch_connected : switchId -> unit Monad.m **)
  
  let handle_switch_connected swId =
    Monad.bind Monad.get (fun st ->
      Monad.bind
        (Monad.put { policy = st.policy; switches = (swId :: st.switches) })
        (fun x ->
        Monad.bind (config_commands st.policy swId) (fun x0 -> Monad.ret ())))
  
  (** val send_output : output -> unit Monad.m **)
  
  let send_output = function
  | OutPkt (swId, pp, pkt, bufOrBytes) ->
    Monad.send swId Word32.zero (PacketOutMsg { pktOutBufOrBytes =
      bufOrBytes; pktOutPortId = None; pktOutActions = ((Output pp) :: []) })
  | OutGetPkt (x, switchId0, portId0, packet0) ->
    Monad.handle_get_packet x switchId0 portId0 packet0
  | OutNothing -> Monad.ret ()
  
  (** val handle_packet_in : switchId -> packetIn -> unit Monad.m **)
  
  let handle_packet_in swId pk =
    Monad.bind Monad.get (fun st ->
      let outs = classify st.policy (packetIn_to_in swId pk) in
      sequence (map send_output outs))
  
  (** val handle_event : event -> unit Monad.m **)
  
  let handle_event = function
  | SwitchConnected swId -> handle_switch_connected swId
  | SwitchDisconnected swId -> handle_switch_disconnected swId
  | SwitchMessage (swId, xid0, msg) ->
    (match msg with
     | PacketInMsg pktIn -> handle_packet_in swId pktIn
     | _ -> Monad.ret ())
  
  (** val main : unit Monad.m **)
  
  let main =
    Monad.forever (Monad.bind Monad.recv (fun evt -> handle_event evt))
 end


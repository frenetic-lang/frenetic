open Classifier
open ControllerInterface0x04
open Datatypes
open List0
open NetCoreEval
open NetCoreEval13
open NetCoreCompiler13
open Packet
open OpenFlow0x04Types
open Types
open WordInterface

(** val prio_rec :
    Word16.t -> 'a1 coq_Classifier -> ((Word16.t*Pattern.pattern)*'a1) list **)

let rec prio_rec prio = function
| [] -> []
| p::rest ->
  let pat,act0 = p in ((prio,pat),act0)::(prio_rec (Word16.pred prio) rest)

(** val prioritize :
    'a1 coq_Classifier -> ((Word16.t*Pattern.pattern)*'a1) list **)

let prioritize lst =
  prio_rec Word16.max_value lst

(** val packetIn_to_in : switchId -> packetIn -> input **)

let packetIn_to_in sw pktIn =
  let inport = List.fold_left (fun acc x -> (match x with 
    | OxmInPort p -> p 
    | _ -> acc)) (Int32.of_int 0) pktIn.pi_ofp_match in
  let pkt = (match pktIn.pi_pkt with
    | Some pkt -> pkt) in
  InPkt (sw, inport, pkt, pktIn.pi_buffer_id)

(** val maybe_openflow0x01_modification :
    'a1 option -> ('a1 -> action) -> actionSequence **)

let maybe_openflow0x01_modification newVal mkModify =
  match newVal with
  | Some v -> (mkModify v)::[]
  | None -> []

(** val modification_to_openflow0x01 : modification -> actionSequence **)

(* TODO: just omitting most mods (NW_SRC etc) because 1.3 parser is a little behind *)
let modification_to_openflow0x01 mods =
  let { modifyDlSrc = dlSrc; modifyDlDst = dlDst; modifyDlVlan = dlVlan;
    modifyDlVlanPcp = dlVlanPcp; modifyNwSrc = nwSrc; modifyNwDst = nwDst;
    modifyNwTos = nwTos; modifyTpSrc = tpSrc; modifyTpDst = tpDst } = mods
  in
  app (maybe_openflow0x01_modification dlSrc (fun x -> SetField (OxmEthSrc (val_to_mask x))))
    (app (maybe_openflow0x01_modification dlDst (fun x -> SetField (OxmEthDst (val_to_mask x))))
        (maybe_openflow0x01_modification (withVlanNone dlVlan) (fun x ->
          SetField (OxmVlanVId (val_to_mask x)))))

(** val translate_action : portId option -> act -> actionSequence **)

let translate_action in_port = function
| Forward (mods, p) ->
  (match p with
   | PhysicalPort pp ->
     app (modification_to_openflow0x01 mods)
       ((match in_port with
         | Some pp' ->
           if pp' = pp
           then Output InPort
           else Output (PhysicalPort pp)
         | None -> Output (PhysicalPort pp))::[])
   | _ -> app (modification_to_openflow0x01 mods) ((Output p)::[]))
| ActGetPkt x -> (Output (Controller Word16.max_value))::[]

(** val to_flow_mod : priority -> Pattern.pattern -> act list -> flowMod **)

let wildcard_to_mask wc def =
  match wc with
    | Wildcard.WildcardExact a -> val_to_mask a
    | Wildcard.WildcardAll -> {value = def; mask = Some def}
    | Wildcard.WildcardNone -> {value = def; mask = Some def}

let pattern_to_oxm_match pat = 
  let { PatternImplDef.ptrnDlSrc = dlSrc;
        ptrnDlDst = dlDst;
        ptrnDlType = dlTyp;
        ptrnDlVlan = dlVlan;
        ptrnDlVlanPcp = dlVlanPcp;
        ptrnNwSrc = nwSrc;
        ptrnNwDst = nwDst;
        ptrnNwProto = nwProto;
        ptrnNwTos = nwTos;
        ptrnTpSrc = tpSrc;
        ptrnTpDst = tpDst;
        ptrnInPort = inPort } = pat in
  (* Is 1 the all wildcard or 0? *)
  ((match dlSrc with Wildcard.WildcardExact a -> [OxmEthSrc (val_to_mask a)] | _ -> [])
   @ (match dlDst with Wildcard.WildcardExact a -> [ OxmEthDst (val_to_mask a)] | _ -> [])
   @ (match dlVlan with Wildcard.WildcardExact a -> [ OxmVlanVId (val_to_mask a)] | _ -> [])
   @ (match nwSrc with Wildcard.WildcardExact a -> [ OxmIP4Src (val_to_mask a)] | _ -> [])
   @ (match nwDst with Wildcard.WildcardExact a -> [ OxmIP4Dst (val_to_mask a)] | _ -> [])
   @ (match inPort with Wildcard.WildcardExact p -> [OxmInPort (Int32.of_int p)] | _ -> [])
   @ (match dlTyp with Wildcard.WildcardExact t -> [OxmEthType t] | _ -> []),
   match inPort with
     | Wildcard.WildcardExact p -> Some (Int32.of_int p)
     | _ -> None)

let to_flow_mod prio pat act0 tableId =
  let ofMatch,inport = pattern_to_oxm_match pat in
  { table_id = tableId; command = AddFlow; ofp_match = ofMatch; priority = prio; 
    instructions = [WriteActions (concat_map (translate_action inport) act0)]; 
    cookie = val_to_mask (Int64.of_int 0); idle_timeout = Permanent; 
    hard_timeout = Permanent; out_group = None;
  flags = {  send_flow_rem = false; 
	     check_overlap = false; 
	     reset_counts = false; 
	     no_pkt_counts = false;
	     no_byt_counts = false }; 
  buffer_id = None; out_port = None}

(** val flow_mods_of_classifier : act list coq_Classifier -> flowMod list **)

let flow_mods_of_classifier lst tblId =
  fold_right (fun ppa lst0 ->
    let p,act0 = ppa in
    let prio,pat = p in
    if Pattern.Pattern.is_empty pat
    then lst0
    else (to_flow_mod prio pat act0 tblId)::lst0) [] (prioritize lst)

(** val delete_all_flows : flowMod **)

let delete_all_flows tableId =
  { command = DeleteFlow; ofp_match = []; priority = 0;
    table_id = tableId; buffer_id = None; out_port = None;
    out_group = None; instructions = []; 
    cookie = val_to_mask (Int64.of_int 0); idle_timeout = Permanent;
    hard_timeout = Permanent;
  flags = {  send_flow_rem = false; 
	     check_overlap = false; 
	     reset_counts = false; 
	     no_pkt_counts = false;
	     no_byt_counts = false }}

type ncstate = { policy : pol; switches : switchId list }

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
  | cmd::lst' -> Monad.bind cmd (fun x -> sequence lst')
  
  (** val config_commands : pol -> switchId -> unit Monad.m **)
  
  let config_commands pol0 swId tblId =
    sequence
      (map (fun fm -> Monad.send swId Word32.zero (FlowMod fm))
        (delete_all_flows tblId::(flow_mods_of_classifier (compile_opt pol0 swId) tblId)))
  
  (** val set_policy : pol -> unit Monad.m **)
  
  (* FIXME: Default tableId of 0 *)
  let set_policy pol0 =
    Monad.bind Monad.get (fun st ->
      let switch_list = st.switches in
      Monad.bind (Monad.put { policy = pol0; switches = switch_list })
        (fun x ->
        Monad.bind (sequence (map (fun sw -> config_commands pol0 sw 0) switch_list))
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
        (Monad.put { policy = st.policy; switches = (swId::st.switches) })
        (fun x ->
        Monad.bind (config_commands st.policy swId 0) (fun x0 -> Monad.ret ())))
  
  (** val send_output : output -> unit Monad.m **)

  let send_output = function
  | OutAct (swId, acts, pkt, bufOrBytes) ->
    let (buf, pkt) = (match bufOrBytes with Coq_inl buf -> (Some buf, None) | _ -> (None, Some pkt)) in
    Monad.send swId Word32.zero (PacketOut { po_buffer_id =
      buf; po_in_port = Controller 0; po_pkt = pkt; po_actions = concat_map (translate_action None) acts })
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
     | PacketIn pktIn -> handle_packet_in swId pktIn
     | _ -> Monad.ret ())
  
  (** val main : unit Monad.m **)
  
  let main =
    Monad.forever (Monad.bind Monad.recv (fun evt -> handle_event evt))
 end


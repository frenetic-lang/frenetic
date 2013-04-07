Set Implicit Arguments.

Require Import Coq.Lists.List.
Require Import Coq.Relations.Relations.
Require Import Common.Types.
Require Import Bag.TotalOrder.
Require Import Bag.Bag2.
Require Import Common.AllDiff.
Require Import Common.Bisimulation.

Local Open Scope list_scope.
Local Open Scope equiv_scope.
Local Open Scope bag_scope.

Module Type NETWORK_ATOMS.

  Parameter packet : Type.
  Parameter switchId : Type.
  Parameter portId : Type.
  Parameter flowTable : Type.
  Parameter flowMod : Type.

  Inductive fromController : Type :=
  | PacketOut : portId -> packet -> fromController
  | BarrierRequest : nat -> fromController
  | FlowMod : flowMod -> fromController.

  Inductive fromSwitch : Type :=
  | PacketIn : portId -> packet -> fromSwitch
  | BarrierReply : nat -> fromSwitch.

  (** Produces a list of packets to forward out of ports, and a list of packets
      to send to the controller. *)
  Parameter process_packet : flowTable -> portId -> packet -> 
    list (portId * packet) * list packet.

  Parameter modify_flow_table : flowMod -> flowTable -> flowTable.

  Parameter packet_le : Relation_Definitions.relation packet.
  Parameter switchId_le : Relation_Definitions.relation switchId.
  Parameter portId_le : Relation_Definitions.relation portId.
  Parameter flowTable_le : Relation_Definitions.relation flowTable.
  Parameter flowMod_le : Relation_Definitions.relation flowMod.
  Parameter fromSwitch_le : Relation_Definitions.relation fromSwitch.
  Parameter fromController_le : Relation_Definitions.relation fromController.

  Declare Instance TotalOrder_packet : TotalOrder packet_le.
  Declare Instance TotalOrder_switchId : TotalOrder switchId_le.
  Declare Instance TotalOrder_portId : TotalOrder portId_le.
  Declare Instance TotalOrder_flowTable : TotalOrder flowTable_le.
  Declare Instance TotalOrder_flowMod : TotalOrder flowMod_le.
  Declare Instance TotalOrder_fromSwitch : TotalOrder fromSwitch_le.
  Declare Instance TotalOrder_fromController : TotalOrder fromController_le.

End NETWORK_ATOMS.

Module Type NETWORK_AND_POLICY <: NETWORK_ATOMS.

  Include Type NETWORK_ATOMS.

  Parameter topo : switchId * portId -> option (switchId * portId).
  Parameter abst_func : switchId -> portId -> packet -> list (portId * packet).

End NETWORK_AND_POLICY.

(** Elements of a Featherweight OpenFlow model. *)
Module Type ATOMS <: NETWORK_AND_POLICY.

  Include NETWORK_AND_POLICY.

  Parameter controller : Type.

  Parameter controller_recv : controller -> switchId -> fromSwitch -> 
    controller -> Prop.

  Parameter controller_step : controller -> controller -> Prop.

  Parameter controller_send : controller ->  controller -> switchId -> 
    fromController -> Prop.

End ATOMS.

Module Type MACHINE.

  Declare Module Atoms : ATOMS.
  Import Atoms.

  Existing Instances TotalOrder_packet TotalOrder_switchId TotalOrder_portId
    TotalOrder_flowTable TotalOrder_flowMod TotalOrder_fromSwitch
    TotalOrder_fromController.

  (* Field names have two purposes. Coq creates selectors with these names,
     and also uses them to generate variable names in proofs. We spend
     more time in FwOF proofs, so we pick short names here. *)
  Record switch := Switch {
    swId : switchId;
    pts : list portId;
    tbl : flowTable;
    inp : bag (PairOrdering portId_le packet_le);
    outp :  bag (PairOrdering portId_le packet_le);
    ctrlm : bag fromController_le;
    switchm : bag fromSwitch_le
  }.

  Inductive switch_le : switch -> switch -> Prop :=
  | SwitchLe : forall sw1 sw2,
      switchId_le (swId sw1) (swId sw2) ->
      switch_le sw1 sw2.

  (* TODO(arjun): fix *)
  Declare Instance TotalOrder_switch : TotalOrder switch_le.
  
  Record dataLink := DataLink {
    src : switchId * portId;
    pks : list packet;
    dst : switchId * portId
  }.
  
  Record openFlowLink := OpenFlowLink {
    of_to : switchId;
    of_switchm : list fromSwitch;
    of_ctrlm : list fromController
  }.

  Definition observation := (switchId * portId * packet) %type.

  (* NOTE(arjun): Ask me in person why exactly I picked these levels. *)
  Reserved Notation "SwitchStep[ sw ; obs ; sw0 ]"
    (at level 70, no associativity).
  Reserved Notation "ControllerOpenFlow[ c ; l ; obs ; c0 ; l0 ]"
    (at level 70, no associativity).
  Reserved Notation "TopoStep[ sw ; link ; obs ; sw0 ; link0 ]"
    (at level 70, no associativity).
  Reserved Notation "SwitchOpenFlow[ s ; l ; obs ; s0 ; l0 ]"
    (at level 70, no associativity).

  Inductive NotBarrierRequest : fromController -> Prop :=
  | PacketOut_NotBarrierRequest : forall pt pk,
      NotBarrierRequest (PacketOut pt pk)
  | FlowMod_NotBarrierRequest : forall fm,
      NotBarrierRequest (FlowMod fm).

  (** Devices of the same type do not interact in a single
      step. Therefore, we never have to permute the lists below. If we
      instead had just one list of all devices, we would have to worry
      about permuting the list or define symmetric step-rules. *)
  Record state := State {
    switches : bag switch_le;
    links : list dataLink;
    ofLinks : list openFlowLink;
    ctrl : controller
  }.

  Inductive step : state -> option observation -> state -> Prop :=
  | PktProcess : forall swId pts tbl pt pk inp outp ctrlm switchm outp'
                        pksToCtrl,
    process_packet tbl pt pk = (outp', pksToCtrl) ->
    SwitchStep[
      Switch swId pts tbl ({|(pt,pk)|} <+> inp) outp ctrlm switchm;
      Some (swId,pt,pk);
      Switch swId pts tbl inp (from_list outp' <+> outp) 
        ctrlm (from_list (map (PacketIn pt) pksToCtrl) <+> switchm)
    ]
  | ModifyFlowTable : forall swId pts tbl inp outp fm ctrlm switchm,
    SwitchStep[
      Switch swId pts tbl inp outp ({|FlowMod fm|} <+> ctrlm) switchm;
      None;
      Switch swId pts (modify_flow_table fm tbl) inp outp ctrlm switchm
    ]
  (** We add the packet to the output-buffer, even if its port is invalid.
      Packets with invalid ports will simply accumulate in the output buffer,
      since the SendDataLink rule only pulls out packets with valid ports.
      This is reasonable for now. The right fix is to add support for OpenFlow
      errors. *)
  | SendPacketOut : forall pt pts swId tbl inp outp pk ctrlm switchm,
    SwitchStep[
      Switch swId pts tbl inp outp  ({|PacketOut pt pk|} <+> ctrlm) switchm;
      None;
      Switch swId pts tbl inp ({| (pt,pk) |} <+> outp) ctrlm switchm
    ]
  | SendDataLink : forall swId pts tbl inp pt pk outp ctrlm switchm pks dst,
    TopoStep[
      Switch swId pts tbl inp ({|(pt,pk)|} <+> outp) ctrlm switchm;
      DataLink (swId,pt) pks dst;
      None;
      Switch swId pts tbl inp outp ctrlm switchm;
      DataLink (swId,pt) (pk :: pks) dst
    ]
  | RecvDataLink : forall swId pts tbl inp outp ctrlm switchm src pks pk pt,
    TopoStep[
      Switch swId pts tbl inp outp ctrlm switchm;
      DataLink src  (pks ++ [pk]) (swId,pt);
      None;
      Switch swId pts tbl ({|(pt,pk)|} <+> inp) outp ctrlm switchm;
      DataLink src pks (swId,pt)
    ]
  | Step_controller : forall sws links ofLinks ctrl ctrl',
    controller_step ctrl ctrl' ->
    step (State sws links ofLinks ctrl)
         None
         (State sws links ofLinks ctrl')
  | ControllerRecv : forall ctrl msg ctrl' swId fromSwitch fromCtrl,
    controller_recv ctrl swId msg ctrl' ->
    ControllerOpenFlow[
      ctrl;
      OpenFlowLink swId (fromSwitch ++ [msg]) fromCtrl;
      None;
      ctrl';
      OpenFlowLink swId fromSwitch fromCtrl
    ]
  | ControllerSend : forall ctrl msg ctrl' swId fromSwitch fromCtrl,
    controller_send ctrl ctrl' swId msg ->
    ControllerOpenFlow[
      ctrl ;
      (OpenFlowLink swId fromSwitch fromCtrl);
      None;
      ctrl';
      (OpenFlowLink swId fromSwitch (msg :: fromCtrl)) ]
  | SendToController : forall swId pts tbl inp outp ctrlm msg switchm fromSwitch
      fromCtrl,
    SwitchOpenFlow[
      Switch swId pts tbl inp outp ctrlm ({| msg |} <+> switchm);
      OpenFlowLink swId fromSwitch fromCtrl;
      None;
      Switch swId pts tbl inp outp ctrlm switchm;
      OpenFlowLink swId (msg :: fromSwitch) fromCtrl
    ]
  | RecvBarrier : forall swId pts tbl inp outp switchm fromSwitch fromCtrl
      xid,
    SwitchOpenFlow[
      Switch swId pts tbl inp outp empty switchm;
      OpenFlowLink swId fromSwitch (fromCtrl ++ [BarrierRequest xid]);
      None;
      Switch swId pts tbl inp outp empty
             ({| BarrierReply xid |} <+> switchm);
      OpenFlowLink swId fromSwitch fromCtrl
    ]
  | RecvFromController : forall swId pts tbl inp outp ctrlm switchm
      fromSwitch fromCtrl msg,
    NotBarrierRequest msg ->
    SwitchOpenFlow[
      Switch swId pts tbl inp outp ctrlm switchm;
      OpenFlowLink swId fromSwitch (fromCtrl ++ [msg]);
      None;
      Switch swId pts tbl inp outp ({| msg |} <+> ctrlm) switchm;
      OpenFlowLink swId fromSwitch fromCtrl
    ]
      where
  "ControllerOpenFlow[ c ; l ; obs ; c0 ; l0 ]" := 
    (forall sws links ofLinks ofLinks',
      step (State sws links (ofLinks ++ l :: ofLinks') c) 
           obs 
           (State sws links (ofLinks ++ l0 :: ofLinks') c0))
    and
  "TopoStep[ sw ; link ; obs ; sw0 ; link0 ]" :=
    (forall sws links links0 ofLinks ctrl,
      step 
      (State (({|sw|}) <+> sws) (links ++ link :: links0) ofLinks ctrl)
      obs
      (State (({|sw0|}) <+> sws) (links ++ link0 :: links0) ofLinks ctrl))
    and
  "SwitchStep[ sw ; obs ; sw0 ]" :=
    (forall sws links ofLinks ctrl,
      step 
        (State (({|sw|}) <+> sws) links ofLinks ctrl)
        obs
        (State (({|sw0|}) <+> sws) links ofLinks ctrl))
    and
  "SwitchOpenFlow[ sw ; of ; obs ; sw0 ; of0 ]" :=
    (forall sws links ofLinks ofLinks0 ctrl,
      step
        (State (({|sw|}) <+> sws) links (ofLinks ++ of :: ofLinks0) ctrl)
        obs
        (State (({|sw0|}) <+> sws) links (ofLinks ++ of0 :: ofLinks0) ctrl)).

  Definition swPtPks : Type :=
    bag (PairOrdering (PairOrdering switchId_le portId_le)
                      packet_le).

  Definition abst_state := swPtPks.

  Definition transfer (sw : switchId) (ptpk : portId * packet) :=
    match ptpk with
      | (pt,pk) =>
        match topo (sw,pt) with
          | Some (sw',pt') => 
            @singleton _ 
               (PairOrdering 
                  (PairOrdering switchId_le portId_le) packet_le)
               (sw',pt',pk) 
          | None => {| |}
        end
    end.

  Definition select_packet_out (sw : switchId) (msg : fromController) :=
    match msg with
      | PacketOut pt pk => transfer sw (pt,pk)
      | _ => {| |}
    end.

  Definition select_packet_in (sw : switchId) (msg : fromSwitch) :=
    match msg with
      | PacketIn pt pk => unions (map (transfer sw) (abst_func sw pt pk))
      | _ => {| |}
    end.

  Definition FlowTableSafe (sw : switchId) (tbl : flowTable) : Prop :=
    forall pt pk forwardedPkts packetIns,
      process_packet tbl pt pk = (forwardedPkts, packetIns) ->
      unions (map (transfer sw) forwardedPkts) <+>
      unions (map (select_packet_in sw) (map (PacketIn pt) packetIns)) =
      unions (map (transfer sw) (abst_func sw pt pk)).

  Inductive NotFlowMod : fromController -> Prop :=
  | NotFlowMod_BarrierRequest : forall n, NotFlowMod (BarrierRequest n)
  | NotFlowMod_PacketOut : forall pt pk, NotFlowMod (PacketOut pt pk).

  Inductive FlowModSafe : switchId -> flowTable -> bag fromController_le -> Prop :=
  | NoFlowModsInBuffer : forall swId tbl ctrlm,
      (forall msg, In msg (to_list ctrlm) -> NotFlowMod msg) ->
      FlowTableSafe swId tbl ->
      FlowModSafe swId tbl ctrlm
  | OneFlowModsInBuffer : forall swId tbl ctrlm f,
      (forall msg, In msg (to_list ctrlm) -> NotFlowMod msg) ->
      FlowTableSafe swId tbl ->
      FlowTableSafe swId (modify_flow_table f tbl) ->
      FlowModSafe swId tbl (({|FlowMod f|}) <+> ctrlm).
 
  Definition FlowTablesSafe (sws : bag switch_le) : Prop :=
    forall swId pts tbl inp outp ctrlm switchm,
      In (Switch swId pts tbl inp outp ctrlm switchm) (to_list sws) ->
      FlowModSafe swId tbl ctrlm.

  Definition SwitchesHaveOpenFlowLinks (sws : bag switch_le) ofLinks :=
    forall sw,
      In sw (to_list sws) ->
      exists ofLink,
        In ofLink ofLinks /\
        swId sw = of_to ofLink.

End MACHINE.

Module Type ATOMS_AND_CONTROLLER.

  Declare Module Machine : MACHINE.
  Import Machine.
  Import Atoms.

  Parameter relate_controller : controller -> swPtPks.

  Parameter ControllerRemembersPackets :
    forall (ctrl ctrl' : controller),
      controller_step ctrl ctrl' ->
      relate_controller ctrl = relate_controller ctrl'.

  Parameter P : bag switch_le -> list openFlowLink -> controller -> Prop.
  
  Parameter P_entails_FlowTablesSafe : forall sws ofLinks ctrl,
    P sws ofLinks ctrl ->
    SwitchesHaveOpenFlowLinks sws ofLinks ->
    FlowTablesSafe sws.
  
  Parameter step_preserves_P : forall sws0 sws1 links0 links1 ofLinks0 ofLinks1 
    ctrl0 ctrl1 obs,
    AllDiff of_to ofLinks0 ->
    AllDiff swId (to_list sws0) ->
    step (State sws0 links0 ofLinks0 ctrl0)
         obs
         (State sws1 links1 ofLinks1 ctrl1) ->
    P sws0 ofLinks0 ctrl0 ->
    P sws1 ofLinks1 ctrl1.

  Parameter ControllerSendForgetsPackets : forall ctrl ctrl' sw msg,
    controller_send ctrl ctrl' sw msg ->
    relate_controller ctrl = select_packet_out sw msg <+>
    relate_controller ctrl'.

  Parameter ControllerRecvRemembersPackets : forall ctrl ctrl' sw msg,
    controller_recv ctrl sw msg ctrl' ->
    relate_controller ctrl' = select_packet_in sw msg <+> 
    (relate_controller ctrl).

  (** If [(sw,pt,pk)] is a packet in the controller's abstract state,
      then the controller will eventually emit the packet. *)
  Parameter ControllerLiveness : forall sw pt pk ctrl0 sws0 links0 
                                        ofLinks0,
    In (sw,pt,pk) (to_list (relate_controller ctrl0)) ->
    exists  ofLinks10 ofLinks11 ctrl1 swTo ptTo switchmLst ctrlmLst,
      (multistep 
         step (State sws0 links0 ofLinks0 ctrl0) nil
         (State sws0 links0
                (ofLinks10 ++ 
                 (OpenFlowLink swTo switchmLst 
                  (PacketOut ptTo pk :: ctrlmLst)) ::
                 ofLinks11) 
                ctrl1)) /\
      select_packet_out swTo (PacketOut ptTo pk) = ({|(sw,pt,pk)|}).

  (** If [m] is a message from the switch to the controller, then the controller
      will eventually consume [m], adding its packet-content to its state. *)
  Parameter ControllerRecvLiveness : forall sws0 links0 ofLinks0 sw switchm0 m 
    ctrlm0 ofLinks1 ctrl0,
     exists ctrl1,
      (multistep 
         step
         (State 
            sws0 links0 
            (ofLinks0 ++ (OpenFlowLink sw (switchm0 ++ [m]) ctrlm0) :: ofLinks1)
            ctrl0)
         nil
         (State 
            sws0 links0 
            (ofLinks0 ++ (OpenFlowLink sw switchm0 ctrlm0) :: ofLinks1)
            ctrl1)) /\
       exists (lps : swPtPks),
         (select_packet_in sw m) <+> lps = relate_controller ctrl1.


End ATOMS_AND_CONTROLLER.

Module Type RELATION_DEFINITIONS.

  Declare Module AtomsAndController : ATOMS_AND_CONTROLLER.
  Import AtomsAndController.
  Import Machine.
  Import Atoms.

  Definition affixSwitch (sw : switchId) (ptpk : portId * packet) :=
    match ptpk with
      | (pt,pk) => (sw,pt,pk)
    end.

  Definition ConsistentDataLinks (links : list dataLink) : Prop :=
    forall (lnk : dataLink),
      In lnk links ->
      topo (src lnk) = Some (dst lnk).

  Definition LinkHasSrc (sws : bag switch_le) (link : dataLink) : Prop :=
    exists switch,
      In switch (to_list sws) /\
      fst (src link) = swId switch /\
      In (snd (src link)) (pts switch).

  Definition LinkHasDst (sws : bag switch_le) (link : dataLink) : Prop :=
    exists switch,
      In switch (to_list sws) /\
      fst (dst link) = swId switch /\
      In (snd (dst link)) (pts switch).

  Definition LinksHaveSrc (sws : bag switch_le) (links : list dataLink) :=
    forall link, In link links -> LinkHasSrc sws link.

  Definition LinksHaveDst (sws : bag switch_le) (links : list dataLink) :=
    forall link, In link links -> LinkHasDst sws link.

  Definition UniqSwIds (sws : bag switch_le) := AllDiff swId (to_list sws).

  Definition ofLinkHasSw (sws : bag switch_le) (ofLink : openFlowLink) :=
    exists sw,
      In sw (to_list sws) /\
      of_to ofLink = swId sw.

  Definition OFLinksHaveSw (sws : bag switch_le) (ofLinks : list openFlowLink) :=
    forall ofLink, In ofLink ofLinks -> ofLinkHasSw sws ofLink.

  Definition DevicesFromTopo (devs : state) :=
    forall swId0 swId1 pt0 pt1,
      Some (swId0,pt0) = topo (swId1,pt1) ->
      exists sw0 sw1 lnk,
        (* TODO(arjun): might as well be lists now. *)
        In sw0 (to_list (switches devs)) /\ 
        In sw1 (to_list (switches devs)) /\
        In lnk (links devs) /\
        swId sw0 = swId0 /\
        swId sw1 = swId1 /\
        src lnk = (swId1,pt1) /\
        dst lnk = (swId0, pt0).

  Definition NoBarriersInCtrlm (sws : bag switch_le) :=
    forall sw,
      In sw (to_list sws) ->
      forall m,
        In m (to_list (ctrlm sw)) ->
        NotBarrierRequest m.

  Record concreteState := ConcreteState {
    devices : state;
    concreteState_flowTableSafety : FlowTablesSafe (switches devices);
    concreteState_consistentDataLinks : ConsistentDataLinks (links devices);
    linksHaveSrc : LinksHaveSrc (switches devices) (links devices);
    linksHaveDst : LinksHaveDst (switches devices) (links devices);
    uniqSwIds : UniqSwIds (switches devices);
    ctrlP : P (switches devices) (ofLinks devices) (ctrl devices);
    uniqOfLinkIds : AllDiff of_to (ofLinks devices);
    ofLinksHaveSw : OFLinksHaveSw (switches devices) (ofLinks devices);
    devicesFromTopo : DevicesFromTopo devices;
    swsHaveOFLinks : SwitchesHaveOpenFlowLinks (switches devices) (ofLinks devices);
    noBarriersInCtrlm : NoBarriersInCtrlm (switches devices)
  }.

  Implicit Arguments ConcreteState [].

  Definition concreteStep (st : concreteState) (obs : option observation)
    (st0 : concreteState) :=
    step (devices st) obs (devices st0).

  Inductive abstractStep : abst_state -> option observation -> abst_state -> 
    Prop := 
  | AbstractStep : forall sw pt pk lps,
    abstractStep
      ({| (sw,pt,pk) |} <+> lps)
      (Some (sw,pt,pk))
      (unions (map (transfer sw) (abst_func sw pt pk)) <+> lps).

  Definition relate_switch (sw : switch) : abst_state :=
    match sw with
      | Switch swId _ tbl inp outp ctrlm switchm =>
        from_list (map (affixSwitch swId) (to_list inp)) <+>
        unions (map (transfer swId) (to_list outp)) <+>
        unions (map (select_packet_out swId) (to_list ctrlm)) <+>
        unions (map (select_packet_in swId) (to_list switchm))
    end.

  Definition relate_dataLink (link : dataLink) : abst_state :=
    match link with
      | DataLink _ pks (sw,pt) =>
        from_list (map (fun pk => (sw,pt,pk)) pks)
    end.

  Definition relate_openFlowLink (link : openFlowLink) : abst_state :=
    match link with
      | OpenFlowLink sw switchm ctrlm =>
        unions (map (select_packet_out sw) ctrlm) <+>
        unions (map (select_packet_in sw) switchm)
    end.

  Definition relate (st : state) : abst_state :=
    unions (map relate_switch (to_list (switches st))) <+>
    unions (map relate_dataLink (links st)) <+>
    unions (map relate_openFlowLink (ofLinks st)) <+>
    relate_controller (ctrl st).

  Definition bisim_relation : relation concreteState abst_state :=
    fun (st : concreteState) (ast : abst_state) => 
      ast = (relate (devices st)).

End RELATION_DEFINITIONS.

Module Type RELATION.

  Declare Module RelationDefinitions : RELATION_DEFINITIONS.
  Import RelationDefinitions.
  Import AtomsAndController.
  Import Machine.
  Import Atoms.

  Parameter simpl_multistep : forall (st1 : concreteState) (devs2 : state) obs,
    multistep step (devices st1) obs devs2 ->
    exists (st2 : concreteState),
      devices st2 = devs2 /\
      multistep concreteStep st1 obs st2.

  Parameter simpl_weak_sim : forall st1 devs2 sw pt pk lps,
    multistep step (devices st1) [(sw,pt,pk)] devs2 ->
    relate (devices st1) = ({| (sw,pt,pk) |} <+> lps) ->
    abstractStep
      ({| (sw,pt,pk) |} <+> lps)
      (Some (sw,pt,pk))
      (unions (map (transfer sw) (abst_func sw pt pk)) <+> lps) ->
   exists st2 : concreteState,
     inverse_relation 
       bisim_relation
       (unions (map (transfer sw) (abst_func sw pt pk)) <+> lps)
       st2 /\
     multistep concreteStep st1 [(sw,pt,pk)] st2.

End RELATION.

Module Type WEAK_SIM_1.

  Declare Module Relation : RELATION.
  Import Relation.
  Import RelationDefinitions.
  Import AtomsAndController.
  Import Machine.
  Import Atoms.

  Parameter weak_sim_1 : weak_simulation concreteStep abstractStep bisim_relation.

End WEAK_SIM_1.

Module Type WEAK_SIM_2.

  Declare Module Relation : RELATION.
  Import Relation.
  Import RelationDefinitions.
  Import AtomsAndController.
  Import Machine.
  Import Atoms.
  
  Parameter weak_sim_2 :
    weak_simulation abstractStep concreteStep (inverse_relation bisim_relation).

End WEAK_SIM_2.

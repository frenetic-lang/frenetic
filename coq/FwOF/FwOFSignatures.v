Set Implicit Arguments.

Require Import Coq.Lists.List.
Require Import Coq.Structures.Equalities.
Require Import Coq.Classes.Equivalence.
Require Import Coq.Classes.EquivDec.
Require Import Common.Types.
Require Import Bag.Bag.
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

  Parameter packet_eq_dec : Eqdec packet.
  Parameter switchId_eq_dec : Eqdec switchId.
  Parameter portId_eq_dec : Eqdec portId.
  Parameter flowTable_eq_dec : Eqdec flowTable.
  Parameter flowMod_eq_dec : Eqdec flowMod.
  Declare Instance Eq_switchId : Eq switchId.
  Declare Instance Eq_portId : Eq portId.
  Declare Instance Eq_packet : Eq packet.

  Instance EqDec_switchId : EqDec switchId eq := eqdec.
  Instance EqDec_portId : EqDec portId eq := eqdec.
  Instance EqDec_packet : EqDec packet eq := eqdec. 

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

  Existing Instances  Eq_switchId Eq_portId Eq_packet EqDec_switchId
     EqDec_portId EqDec_packet.

  Parameter fromController_eq_dec : Eqdec fromController.
  Parameter fromSwitch_eq_dec : Eqdec fromSwitch.
  Declare Instance FromController_Eq : Eq fromController.
  Declare Instance FromSwitch_Eq : Eq fromSwitch.
  Declare Instance FlowTable_Eq : Eq flowTable.
  Instance fromController_eqdec : EqDec fromController eq := eqdec.
  Instance fromSwitch_eqdec : EqDec fromSwitch eq := eqdec.

  (* Field names have two purposes. Coq creates selectors with these names,
     and also uses them to generate variable names in proofs. We spend
     more time in FwOF proofs, so we pick short names here. *)
  Record switch := Switch {
    swId : switchId;
    pts : list portId;
    tbl : flowTable;
    inp : bag (portId * packet);
    outp :  bag (portId * packet);
    ctrlm : bag fromController;
    switchm : bag fromSwitch
  }.
  
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
    switches : bag switch;
    links : list dataLink;
    ofLinks : list openFlowLink;
    ctrl : controller
  }.

  (** Switches contain bags and bags do not have unique representations. In
      proofs, it is common to replace a bag with an equivalent (but unequal)
      bag. When we do, we need to replace the switch with an equivalent switch
      too. *)

  Inductive switch_equiv : switch -> switch -> Prop :=
  | SwitchEquiv : forall swId pts tbl inp inp' outp outp' ctrlm ctrlm'
                         switchm switchm',
    inp === inp' ->
    outp === outp' ->
    ctrlm  === ctrlm' ->
    switchm === switchm' ->
    switch_equiv (Switch swId pts tbl inp outp ctrlm switchm)
                 (Switch swId pts tbl inp' outp' ctrlm' switchm').

  Parameter switch_equiv_is_Equivalence : Equivalence switch_equiv.
  
  Instance switch_Equivalence : Equivalence switch_equiv := 
    switch_equiv_is_Equivalence.
  
  Parameter switch_eqdec : EqDec switch switch_equiv.
  
  Inductive stateEquiv : state -> state -> Prop :=
  | StateEquiv : forall sws1 sws2 links ofLinks ctrl,
    sws1 === sws2 ->
    stateEquiv
      (State sws1 links ofLinks ctrl) 
      (State sws2 links ofLinks ctrl).
  
  Parameter stateEquiv_is_Equivalence : Equivalence stateEquiv.

  Instance stateEquiv_Equivalence : Equivalence stateEquiv :=
    stateEquiv_is_Equivalence.
    
  Inductive step : state -> option observation -> state -> Prop :=
  | StepEquivState : forall st1 st2,
    st1 === st2 ->
    step st1 None st2
  | PktProcess : forall swId pts tbl pt pk inp outp ctrlm switchm outp'
                        pksToCtrl,
    process_packet tbl pt pk = (outp', pksToCtrl) ->
    SwitchStep[
      Switch swId pts tbl ({|(pt,pk)|} <+> inp) outp ctrlm switchm;
      Some (swId,pt,pk);
      Switch swId pts tbl inp (FromList outp' <+> outp) 
        ctrlm (FromList (map (PacketIn pt) pksToCtrl) <+> switchm)
    ]
  | ModifyFlowTable : forall swId pts tbl inp outp fm ctrlm switchm,
    SwitchStep[
      Switch swId pts tbl inp outp ({|FlowMod fm|} <+> ctrlm) switchm;
      None;
      Switch swId pts (modify_flow_table fm tbl) inp outp ctrlm switchm
    ]
  | SendPacketOut : forall pt pts swId tbl inp outp pk ctrlm switchm,
    In pt pts ->
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
      Switch swId pts tbl inp outp Empty switchm;
      OpenFlowLink swId fromSwitch (fromCtrl ++ [BarrierRequest xid]);
      None;
      Switch swId pts tbl inp outp Empty
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

  Definition abst_state := bag (switchId * portId * packet).

  Definition transfer (sw : switchId) (ptpk : portId * packet) :=
    match ptpk with
      | (pt,pk) =>
        match topo (sw,pt) with
          | Some (sw',pt') => {| (sw',pt',pk) |}
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
      | PacketIn pt pk => Bag.unions (map (transfer sw) (abst_func sw pt pk))
      | _ => {| |}
    end.

  Definition FlowTableSafe (sw : switchId) (tbl : flowTable) : Prop :=
    forall pt pk forwardedPkts packetIns,
      process_packet tbl pt pk = (forwardedPkts, packetIns) ->
      Bag.unions (map (transfer sw) forwardedPkts) <+>
      Bag.unions (map (select_packet_in sw) (map (PacketIn pt) packetIns)) ===
      Bag.unions (map (transfer sw) (abst_func sw pt pk)).

  Section FlowModSafety.

    Inductive Endpoint : Type :=
    | Endpoint_NoBarrier : flowTable -> Endpoint
    | Endpoint_Barrier : flowTable -> Endpoint.

    Definition table_at_endpoint (ep : Endpoint) :=
      match ep with
        | Endpoint_NoBarrier tbl => tbl
        | Endpoint_Barrier tbl => tbl
      end.

    Inductive SafeWire : switchId -> 
                    Endpoint -> 
                    list fromController ->
                    Endpoint -> Prop :=
    | SafeWire_nil : forall swId ep,
      FlowTableSafe swId (table_at_endpoint ep) ->
      SafeWire swId ep nil ep
    | SafeWire_PktOut : forall swId pt pk ctrlEp ctrlm swEp,
      SafeWire swId ctrlEp ctrlm swEp ->
      SafeWire swId ctrlEp (PacketOut pt pk :: ctrlm) swEp
    | SafeWire_BarrierRequest : forall swId n ctrlEp ctrlm swEp,
      SafeWire swId ctrlEp ctrlm swEp ->
      SafeWire swId (Endpoint_Barrier (table_at_endpoint ctrlEp))
                    (BarrierRequest n :: ctrlm)
                    swEp
    | SafeWire_FlowMod : forall swId f tbl ctrlm swEp,
      FlowTableSafe swId (modify_flow_table f tbl) ->
      SafeWire swId (Endpoint_Barrier tbl) ctrlm swEp ->
      SafeWire swId (Endpoint_NoBarrier (modify_flow_table f tbl))
          (FlowMod f :: ctrlm) swEp.

    Inductive NotFlowMod : fromController -> Prop :=
    | NotFlowMod_BarrierRequest : forall n, NotFlowMod (BarrierRequest n)
    | NotFlowMod_PacketOut : forall pt pk, NotFlowMod (PacketOut pt pk).

    Inductive SwitchEP : switch -> Endpoint -> Prop :=
    | NoFlowModsInBuffer : forall swId pts tbl inp outp ctrlm switchm swEp,
      (forall msg, Mem msg ctrlm -> NotFlowMod msg) ->
      table_at_endpoint swEp = tbl ->
      SwitchEP
        (Switch swId pts tbl inp outp ctrlm switchm)
        swEp
    | OneFlowModInBuffer : forall swId pts tbl inp outp ctrlm ctrlm0 switchm f,
      (forall msg, Mem msg ctrlm0 -> NotFlowMod msg) ->
      ctrlm === ({|FlowMod f|} <+> ctrlm0) ->
      FlowTableSafe swId (modify_flow_table f tbl) ->
      SwitchEP (Switch swId pts tbl inp outp ctrlm switchm)
               (Endpoint_NoBarrier (modify_flow_table f tbl)).

    (** "FMS" is short for "flow mod safety". *)
    Inductive FMS : switch -> openFlowLink -> Prop := 
    | MkFMS : forall swId pts tbl inp outp ctrlm switchm ctrlmList switchmList
                     switchEp,
      SwitchEP (Switch swId pts tbl inp outp ctrlm switchm) switchEp ->
      (exists ctrlEp, SafeWire swId ctrlEp ctrlmList switchEp) ->
      FMS 
        (Switch swId pts tbl inp outp ctrlm switchm)
        (OpenFlowLink swId switchmList ctrlmList).

    Definition AllFMS (sws : bag switch) (ofLinks : list openFlowLink) :=
      forall sw,
        Mem sw sws ->
        exists lnk, 
          In lnk ofLinks /\
          of_to lnk = swId sw /\
          FMS sw lnk.

    Parameter SwitchEP_equiv : forall swId pts tbl inp0 inp1 outp0 outp1
      ctrlm0 ctrlm1 switchm0 switchm1 ep,
      ctrlm0 === ctrlm1 ->
      SwitchEP (Switch swId pts tbl inp0 outp0 ctrlm0 switchm0) ep ->
      SwitchEP (Switch swId pts tbl inp1 outp1 ctrlm1 switchm1) ep.

    Parameter FMS_untouched : forall swId0 pts0 tbl0 inp0 outp0 ctrlm0 switchm0
      switchmLst0 ctrlmLst0 inp1 outp1 switchm1 switchmLst1,
      FMS (Switch swId0 pts0 tbl0 inp0 outp0 ctrlm0 switchm0)
          (OpenFlowLink swId0 switchmLst0 ctrlmLst0) ->
      FMS (Switch swId0 pts0 tbl0 inp1 outp1 ctrlm0 switchm1)
          (OpenFlowLink swId0 switchmLst1 ctrlmLst0).

    Parameter FMS_equiv : forall sw1 sw2 lnk,
      sw1 === sw2 ->
      FMS sw1 lnk ->
      FMS sw2 lnk.

    Parameter SafeWire_dequeue_PacketOut : forall sw ctrlEp ctrlLst pt pk 
      switchEp,
      SafeWire sw ctrlEp (ctrlLst ++ [PacketOut pt pk]) switchEp ->
      SafeWire sw ctrlEp (ctrlLst) switchEp.

    Parameter SafeWire_dequeue_BarrierRequest : forall sw ctrlEp ctrlLst xid 
      switchEp1,
      SafeWire sw ctrlEp (ctrlLst ++ [BarrierRequest xid]) switchEp1 ->
      exists switchEp2, SafeWire sw ctrlEp (ctrlLst) switchEp2 /\
        table_at_endpoint switchEp2 = table_at_endpoint switchEp1.

    Parameter SafeWire_dequeue_safe : forall sw ctrlEp ctrlLst f switchEp,
       SafeWire sw ctrlEp (ctrlLst ++ [FlowMod f]) switchEp ->
       FlowTableSafe sw (modify_flow_table f (table_at_endpoint switchEp)).

    Parameter SafeWire_dequeue_FlowMod : forall sw ctrlEp ctrlLst f switchEp,
       SafeWire sw ctrlEp (ctrlLst ++ [FlowMod f]) switchEp ->
       SafeWire sw ctrlEp ctrlLst 
                (Endpoint_NoBarrier 
                   (modify_flow_table f
                     (table_at_endpoint switchEp))).

    Parameter SafeWire_fm_nb_false : forall sw ctrlEp ctrlLst f tbl,
      SafeWire sw ctrlEp (ctrlLst ++ [FlowMod f]) (Endpoint_NoBarrier tbl) ->
      False.

    Parameter FMS_pop : forall sw pts tbl inp outp ctrlm switchm switchLst 
                               ctrlLst msg,
      FMS  (Switch sw pts tbl inp outp ctrlm switchm) 
           (OpenFlowLink sw switchLst (ctrlLst ++ [msg])) ->
      FMS  (Switch sw pts tbl inp outp ({|msg|} <+> ctrlm) switchm) 
           (OpenFlowLink sw switchLst ctrlLst).

    Parameter FMS_dequeue_pktOut : forall sw pts tbl inp outp ctrlm switchm 
                                      switchLst ctrlLst pt pk,
      FMS  (Switch sw pts tbl inp outp ({|PacketOut pt pk|} <+> ctrlm) switchm)
           (OpenFlowLink sw switchLst ctrlLst) ->
      FMS  (Switch sw pts tbl inp ({|(pt,pk)|} <+> outp) ctrlm switchm) 
           (OpenFlowLink sw switchLst ctrlLst).

  End FlowModSafety.

End MACHINE.

Module Type ATOMS_AND_CONTROLLER.

  Declare Module Machine : MACHINE.
  Import Machine.
  Import Atoms.

  Parameter relate_controller : controller -> bag (switchId * portId * packet).

  Parameter ControllerRemembersPackets :
    forall (ctrl ctrl' : controller),
      controller_step ctrl ctrl' ->
      relate_controller ctrl = relate_controller ctrl'.

  Parameter P : bag switch -> list openFlowLink -> controller -> Prop.
  
  Parameter step_preserves_P : forall sws0 sws1 links0 links1 ofLinks0 ofLinks1 
    ctrl0 ctrl1 obs,
    step (State sws0 links0 ofLinks0 ctrl0)
         obs
         (State sws1 links1 ofLinks1 ctrl1) ->
    P sws0 ofLinks0 ctrl0 ->
    P sws1 ofLinks1 ctrl1.

  Parameter ControllerSendForgetsPackets : forall ctrl ctrl' sw msg,
    controller_send ctrl ctrl' sw msg ->
    relate_controller ctrl === select_packet_out sw msg <+>
    relate_controller ctrl'.

  Parameter ControllerRecvRemembersPackets : forall ctrl ctrl' sw msg,
    controller_recv ctrl sw msg ctrl' ->
    relate_controller ctrl' === select_packet_in sw msg <+> 
    (relate_controller ctrl).

  (** If [(sw,pt,pk)] is a packet in the controller's abstract state, then the
      controller will eventually emit the packet. *)
  Parameter ControllerLiveness : forall sw pt pk ctrl0 sws0 links0 ofLinks0,
    Mem (sw,pt,pk) (relate_controller ctrl0) ->
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
       exists (lps : bag (switchId * portId * packet)),
         (select_packet_in sw m) <+> lps === relate_controller ctrl1.

  Parameter ControllerFMS : forall swId ctrl0 ctrl1 msg ctrlm
    switchm sws links ofLinks0 ofLinks1 switchEp
    pts tbl inp outp swCtrlm swSwitchm,
    P sws
      (ofLinks0 ++ (OpenFlowLink swId switchm ctrlm) :: ofLinks1)
      ctrl0 ->
    controller_send ctrl0 ctrl1 swId msg ->
    step
      (State
        sws
        links
        (ofLinks0 ++ (OpenFlowLink swId switchm ctrlm) :: ofLinks1)
        ctrl0)
      None
      (State
         sws
         links
         (ofLinks0 ++ (OpenFlowLink swId switchm (msg :: ctrlm)) :: ofLinks1)
         ctrl1) ->
     Mem (Switch swId pts tbl inp outp swCtrlm swSwitchm) sws ->
     SwitchEP (Switch swId pts tbl inp outp swCtrlm swSwitchm) switchEp ->
      exists ctrlEp1,
        SafeWire swId ctrlEp1 (msg :: ctrlm) switchEp.

End ATOMS_AND_CONTROLLER.

Module Type RELATION.

  Declare Module AtomsAndController : ATOMS_AND_CONTROLLER.
  Export AtomsAndController.
  Export Machine.
  Export Atoms.

  Definition affixSwitch (sw : switchId) (ptpk : portId * packet) :=
    match ptpk with
      | (pt,pk) => (sw,pt,pk)
    end.

  Definition FlowTablesSafe (sws : bag switch) : Prop :=
    forall swId pts tbl inp outp ctrlm switchm,
      Mem (Switch swId pts tbl inp outp ctrlm switchm) sws ->
      FlowTableSafe swId tbl.

  Definition ConsistentDataLinks (links : list dataLink) : Prop :=
    forall (lnk : dataLink),
      In lnk links ->
      topo (src lnk) = Some (dst lnk).

  Definition LinkHasSrc (sws : bag switch) (link : dataLink) : Prop :=
    exists switch,
      Mem switch sws /\
      fst (src link) = swId switch /\
      In (snd (src link)) (pts switch).

  Definition LinkHasDst (sws : bag switch) (link : dataLink) : Prop :=
    exists switch,
      Mem switch sws /\
      fst (dst link) = swId switch /\
      In (snd (dst link)) (pts switch).

  Definition LinksHaveSrc (sws : bag switch) (links : list dataLink) :=
    forall link, In link links -> LinkHasSrc sws link.

  Definition LinksHaveDst (sws : bag switch) (links : list dataLink) :=
    forall link, In link links -> LinkHasDst sws link.

  Definition UniqSwIds (sws : bag switch) := AllDiff swId (Bag.to_list sws).

  Definition ofLinkHasSw (sws : bag switch) (ofLink : openFlowLink) :=
    exists sw,
      Mem sw sws /\
      of_to ofLink = swId sw.

  Definition OFLinksHaveSw (sws : bag switch) (ofLinks : list openFlowLink) :=
    forall ofLink, In ofLink ofLinks -> ofLinkHasSw sws ofLink.

  Definition DevicesFromTopo (devs : state) :=
    forall swId0 swId1 pt0 pt1,
      Some (swId0,pt0) = topo (swId1,pt1) ->
      exists sw0 sw1 lnk,
        Mem sw0 (switches devs) /\
        Mem sw1 (switches devs) /\
        In lnk (links devs) /\
        swId sw0 = swId0 /\
        swId sw1 = swId1 /\
        src lnk = (swId1,pt1) /\
        dst lnk = (swId0, pt0).

  Definition SwitchesHaveOpenFlowLinks (devs : state) :=
    forall sw,
      Mem sw (switches devs) ->
      exists ofLink,
        In ofLink (ofLinks devs) /\
        swId sw = of_to ofLink.

  Record concreteState := ConcreteState {
    devices : state;
    concreteState_flowTableSafety : FlowTablesSafe (switches devices);
    concreteState_consistentDataLinks : ConsistentDataLinks (links devices);
    linksHaveSrc : LinksHaveSrc (switches devices) (links devices);
    linksHaveDst : LinksHaveDst (switches devices) (links devices);
    uniqSwIds : UniqSwIds (switches devices);
    allFMS : AllFMS (switches devices) (ofLinks devices);
    ctrlP : P (switches devices) (ofLinks devices) (ctrl devices);
    uniqOfLinkIds : AllDiff of_to (ofLinks devices);
    ofLinksHaveSw : OFLinksHaveSw (switches devices) (ofLinks devices);
    devicesFromTopo : DevicesFromTopo devices;
    swsHaveOFLinks : SwitchesHaveOpenFlowLinks devices
  }.

  Implicit Arguments ConcreteState [].

  Definition concreteStep (st : concreteState) (obs : option observation)
    (st0 : concreteState) :=
    step (devices st) obs (devices st0).

  Inductive abstractStep : abst_state -> option observation -> abst_state -> 
    Prop := 
  | AbstractStepEquiv : forall st st',
      st === st' ->
      abstractStep st None st'
  | AbstractStep : forall sw pt pk lps,
    abstractStep
      ({| (sw,pt,pk) |} <+> lps)
      (Some (sw,pt,pk))
      (Bag.unions (map (transfer sw) (abst_func sw pt pk)) <+> lps).

  Definition relate_switch (sw : switch) : abst_state :=
    match sw with
      | Switch swId _ tbl inp outp ctrlm switchm =>
        FromList (map (affixSwitch swId) (Bag.to_list inp)) <+>
        Bag.unions (map (transfer swId) (Bag.to_list outp)) <+>
        Bag.unions (map (select_packet_out swId) (Bag.to_list ctrlm)) <+>
        Bag.unions (map (select_packet_in swId) (Bag.to_list switchm))
    end.

  Definition relate_dataLink (link : dataLink) : abst_state :=
    match link with
      | DataLink _ pks (sw,pt) =>
        FromList (map (fun pk => (sw,pt,pk)) pks)
    end.

  Definition relate_openFlowLink (link : openFlowLink) : abst_state :=
    match link with
      | OpenFlowLink sw switchm ctrlm =>
        Bag.unions (map (select_packet_out sw) ctrlm) <+>
        Bag.unions (map (select_packet_in sw) switchm)
    end.


  Definition relate (st : state) : abst_state :=
    Bag.unions (map relate_switch (Bag.to_list (switches st))) <+>
    Bag.unions (map relate_dataLink (links st)) <+>
    Bag.unions (map relate_openFlowLink (ofLinks st)) <+>
    relate_controller (ctrl st).

  Definition bisim_relation : relation concreteState abst_state :=
    fun (st : concreteState) (ast : abst_state) => 
      ast === (relate (devices st)).

  Parameter simpl_multistep : forall (st1 st2 : state) obs
    (tblsOk1 : FlowTablesSafe (switches st1))
    (linksTopoOk1 : ConsistentDataLinks (links st1))
    (haveSrc1 : LinksHaveSrc (switches st1) (links st1))
    (haveDst1 : LinksHaveDst (switches st1) (links st1))
    (uniqSwIds1 : UniqSwIds (switches st1))
    (allFMS1 : AllFMS (switches st1) (ofLinks st1))
    (P1 : P (switches st1) (ofLinks st1) (ctrl st1))
    (uniqOfLinkIds1 : AllDiff of_to (ofLinks st1))
    (ofLinksHaveSw1 : OFLinksHaveSw (switches st1) (ofLinks st1))
    (devsFromTopo1 : DevicesFromTopo st1)
    (swsHaveOFLinks1 : SwitchesHaveOpenFlowLinks st1),
    multistep step st1 obs st2 ->
    exists tblsOk2 linksTopoOk2 haveSrc2 haveDst2 uniqSwIds2 allFMS2 P2
           uniqOfLinkIds2 ofLinksHaveSw2 devsFromTopo2 swsHaveOFLinks2,
      multistep concreteStep
                (ConcreteState st1 tblsOk1 linksTopoOk1 haveSrc1 haveDst1 
                               uniqSwIds1 allFMS1 P1 uniqOfLinkIds1
                               ofLinksHaveSw1 devsFromTopo1 swsHaveOFLinks1)
                obs
                (ConcreteState st2 tblsOk2 linksTopoOk2 haveSrc2 haveDst2 
                               uniqSwIds2 allFMS2 P2 uniqOfLinkIds2
                               ofLinksHaveSw2 devsFromTopo2 swsHaveOFLinks2).


  Parameter simpl_weak_sim : forall devs1 devs2 sw pt pk lps
    (tblsOk1 : FlowTablesSafe (switches devs1))
    (linksTopoOk1 : ConsistentDataLinks (links devs1))
    (haveSrc1 : LinksHaveSrc (switches devs1) (links devs1))
    (haveDst1 : LinksHaveDst (switches devs1) (links devs1))
    (uniqSwIds1 : UniqSwIds (switches devs1))
    (allFMS1 : AllFMS (switches devs1) (ofLinks devs1))
    (P1 : P (switches devs1) (ofLinks devs1) (ctrl devs1))
    (uniqOfLinkIds1 : AllDiff of_to (ofLinks devs1))
    (ofLinksHaveSw1 : OFLinksHaveSw (switches devs1) (ofLinks devs1))
    (devsFromTopo1 : DevicesFromTopo devs1)
    (swsHaveOFLinks1 : SwitchesHaveOpenFlowLinks devs1),
    multistep step devs1 [(sw,pt,pk)] devs2 ->
    relate devs1 === ({| (sw,pt,pk) |} <+> lps) ->
    abstractStep
      ({| (sw,pt,pk) |} <+> lps)
      (Some (sw,pt,pk))
      (Bag.unions (map (transfer sw) (abst_func sw pt pk)) <+> lps) ->
   exists t : concreteState,
     inverse_relation 
       bisim_relation
       (Bag.unions (map (transfer sw) (abst_func sw pt pk)) <+> lps)
       t /\
     multistep concreteStep
               (ConcreteState devs1 tblsOk1 linksTopoOk1 haveSrc1 haveDst1
                              uniqSwIds1 allFMS1 P1 uniqOfLinkIds1
                              ofLinksHaveSw1 devsFromTopo1 swsHaveOFLinks1)
               [(sw,pt,pk)]
               t.

  Parameter FlowTablesSafe_untouched : forall {sws swId pts tbl inp inp'
    outp outp' ctrlm ctrlm' switchm switchm' },
    FlowTablesSafe
      ({|Switch swId pts tbl inp outp ctrlm switchm|} <+> sws) ->
    FlowTablesSafe 
      ({|Switch swId pts tbl inp' outp' ctrlm' switchm'|} <+> sws).

  Parameter LinksHaveSrc_untouched : forall 
    {swId tbl pts sws links
    inp outp ctrlm switchm tbl' inp' outp' ctrlm' switchm' },
    LinksHaveSrc 
      ({| Switch swId pts tbl inp outp ctrlm switchm |} <+> sws)  links ->
    LinksHaveSrc 
      ({| Switch swId pts tbl' inp' outp' ctrlm' switchm' |} <+> sws)
      links.

  Parameter LinkHasSrc_equiv : forall {sws sws' link},
    sws === sws' ->
    LinkHasSrc sws link ->
    LinkHasSrc sws' link.

  Parameter LinksHaveDst_untouched : forall 
    {swId tbl pts sws links
    inp outp ctrlm switchm tbl' inp' outp' ctrlm' switchm' },
    LinksHaveDst
      ({| Switch swId pts tbl inp outp ctrlm switchm |} <+>  sws)  links ->
    LinksHaveDst 
      ({| Switch swId pts tbl' inp' outp' ctrlm' switchm' |} <+> sws)
      links.

  Parameter LinkHasDst_equiv : forall {sws sws' link},
    sws === sws' ->
    LinkHasDst sws link ->
    LinkHasDst sws' link.

End RELATION.

Module Type WEAK_SIM_1.

  Declare Module Relation : RELATION.
  Import Relation.
  
  Parameter weak_sim_1 : weak_simulation concreteStep abstractStep bisim_relation.

End WEAK_SIM_1.

Module Type WEAK_SIM_2.

  Declare Module Relation : RELATION.
  Import Relation.
  
  Parameter weak_sim_2 :
    weak_simulation abstractStep concreteStep (inverse_relation bisim_relation).

End WEAK_SIM_2.

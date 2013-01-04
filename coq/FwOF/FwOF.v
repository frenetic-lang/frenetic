Set Implicit Arguments.

Require Import Coq.Lists.List.
Require Import Coq.Structures.Equalities.
Require Import Common.Types.
Require Import Bag.Bag.

Local Open Scope list_scope.
Local Open Scope equiv_scope.
Local Open Scope bag_scope.

(** Elements of a Featherweight OpenFlow model. *)
Module Type ATOMS.

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

  Section Controller.

    Parameter controller : Type.

    Parameter controller_recv : controller -> switchId -> fromSwitch -> 
      controller -> Prop.

    Parameter controller_step : controller -> controller -> Prop.

    Parameter controller_send : controller ->  controller -> switchId -> 
      fromController -> Prop.

  End Controller.

End ATOMS.

Module ConcreteSemantics (Import Atoms : ATOMS).

  Section DecidableEqualities.

    Hint Resolve packet_eq_dec switchId_eq_dec portId_eq_dec flowTable_eq_dec
      flowMod_eq_dec.

    Lemma fromController_eq_dec : Eqdec fromController.
    Proof.
      unfold Eqdec. decide equality. apply eqdec.
    Qed.

    Lemma fromSwitch_eq_dec : Eqdec fromSwitch.
    Proof.
      unfold Eqdec. decide equality. apply eqdec.
    Qed.

  End DecidableEqualities.

  Instance Packet_Eq : Eq packet.
  Proof.
    split. apply packet_eq_dec.
  Qed.

  Instance PortId_Eq : Eq portId.
  Proof.
    split. apply portId_eq_dec.
  Qed.

  Instance SwitchId_Eq : Eq switchId.
  Proof.
    split. apply switchId_eq_dec.
  Qed.

  Instance FromController_Eq : Eq fromController.
  Proof.
    split. apply fromController_eq_dec.
  Qed.

  Instance FromSwitch_Eq : Eq fromSwitch.
  Proof.
    split. apply fromSwitch_eq_dec.
  Qed.

  
  Record switch := Switch {
    switch_swichId : switchId;
    switch_ports : list portId;
    switch_flowTable : flowTable;
    switch_inputPackets : Bag.bag (portId * packet);
    switch_outputPackets :  Bag.bag (portId * packet);
    switch_fromController : Bag.bag fromController;
    switch_fromSwitch : Bag.bag fromSwitch
  }.
  
  Record dataLink := DataLink {
    dataLink_src : switchId * portId;
    dataList_packets : list packet;
    dataLink_dst : switchId * portId
  }.
  
  Record openFlowLink := OpenFlowLink {
    openFlowLink_to : switchId;
    openFlowLink_fromSwitch : list fromSwitch;
    openFlowLink_fromController : list fromController
  }.

  Definition observation := (switchId * portId * packet) %type.

  Inductive SwitchStep : option observation -> switch -> switch -> Prop :=
  | PktProcess : forall swId pts tbl pt pk inp outp ctrlm switchm outp'
                        pksToCtrl,
    process_packet tbl pt pk = (outp', pksToCtrl) ->
    SwitchStep
      (Some (swId,pt,pk))
      (Switch swId pts tbl ({|(pt,pk)|} <+> inp) outp 
         ctrlm switchm)
      (Switch swId pts tbl inp (Bag.FromList outp' <+> outp) 
         ctrlm (Bag.FromList (map (PacketIn pt) pksToCtrl) <+> switchm))
  | ModifyFlowTable : forall swId pts tbl inp outp fm ctrlm switchm,
    SwitchStep
      None
      (Switch swId pts tbl inp outp 
         ({|FlowMod fm|} <+> ctrlm) switchm)
      (Switch swId pts (modify_flow_table fm tbl) inp outp 
         ctrlm switchm)
  | SendPacketOut : forall pt pts swId tbl inp outp pk ctrlm switchm,
    In pt pts ->
    SwitchStep
      None
      (Switch swId pts tbl inp outp  ({|PacketOut pt pk|} <+> ctrlm) switchm)
      (Switch swId pts tbl inp ({| (pt,pk) |} <+> outp) ctrlm switchm).

  Inductive TopoStep : switch -> dataLink -> switch -> dataLink -> Prop :=
  | SendDataLink : forall swId pts tbl inp pt pk outp ctrlm switchm pks dst,
    TopoStep
      (Switch swId pts tbl inp ({|(pt,pk)|} <+> outp) ctrlm switchm)
      (DataLink (swId,pt) pks dst)
      (Switch swId pts tbl inp outp ctrlm switchm)
      (DataLink (swId,pt) (pk :: pks) dst)
  | RecvDataLink : forall swId pts tbl inp outp ctrlm switchm src pks pk pt,
    TopoStep
      (Switch swId pts tbl inp outp ctrlm switchm)
      (DataLink src  (pks ++ [pk]) (swId,pt))
      (Switch swId pts tbl ({|(pt,pk)|} <+> inp) outp ctrlm switchm)
      (DataLink src pks (swId,pt)).


  Inductive ControllerOpenFlowStep : controller -> openFlowLink 
    -> controller -> openFlowLink -> Prop :=
  | ControllerRecv : forall ctrl msg ctrl' swId fromSwitch fromCtrl,
    controller_recv ctrl swId msg ctrl' ->
    ControllerOpenFlowStep
      ctrl
      (OpenFlowLink swId (fromSwitch ++ [msg]) fromCtrl)
      ctrl'
      (OpenFlowLink swId fromSwitch fromCtrl)
  | ControllerSend : forall ctrl msg ctrl' swId fromSwitch fromCtrl,
    controller_send ctrl ctrl' swId msg ->
    ControllerOpenFlowStep
      ctrl
      (OpenFlowLink swId fromSwitch fromCtrl)
      ctrl'
      (OpenFlowLink swId fromSwitch (msg :: fromCtrl)).

  Inductive NotBarrierRequest : fromController -> Prop :=
  | PacketOut_NotBarrierRequest : forall pt pk,
      NotBarrierRequest (PacketOut pt pk)
  | FlowMod : forall fm,
      NotBarrierRequest (FlowMod fm).

  Inductive SwitchOpenFlowStep : switch -> openFlowLink 
    -> switch -> openFlowLink -> Prop :=
  | SendToController : forall swId pts tbl inp outp ctrlm msg switchm fromSwitch
      fromCtrl,
    SwitchOpenFlowStep
      (Switch swId pts tbl inp outp ctrlm ({| msg |} <+> switchm))
      (OpenFlowLink swId fromSwitch fromCtrl)
      (Switch swId pts tbl inp outp ctrlm switchm)
      (OpenFlowLink swId (msg :: fromSwitch) fromCtrl)
  | RecvBarrier : forall swId pts tbl inp outp switchm fromSwitch fromCtrl
      xid,
    SwitchOpenFlowStep
      (Switch swId pts tbl inp outp 
        Bag.Empty 
        switchm)
      (OpenFlowLink swId fromSwitch (fromCtrl ++ [BarrierRequest xid]))
      (Switch swId pts tbl inp outp 
         Bag.Empty
         ({| BarrierReply xid |} <+> switchm))
      (OpenFlowLink swId fromSwitch fromCtrl)
  | RecvFromController : forall swId pts tbl inp outp ctrlm switchm
      fromSwitch fromCtrl (msg : fromController),
    NotBarrierRequest msg ->
    SwitchOpenFlowStep
      (Switch swId pts tbl inp outp ctrlm switchm)
      (OpenFlowLink swId fromSwitch (fromCtrl ++ [msg]))
      (Switch swId pts tbl inp outp ({| msg |} <+> ctrlm) switchm)
      (OpenFlowLink swId fromSwitch fromCtrl).

  Record state := State {
    state_switches : list switch;
    state_dataLinks : list dataLink;
    state_openFlowLinks : list openFlowLink;
    state_controller : controller
  }.
    
  (** TODO(arjun): We're getting very little by separating out
      SwitchStep, TopoStep, etc.  I think it's actually more annoying
      this way. *)
  Inductive step : state -> option observation -> state -> Prop :=
  | Step_SwitchStep : forall obs sw sw' sws sws0 links ofLinks ctrl,
      SwitchStep obs sw sw' ->
      step (State (sws ++ sw :: sws0) links ofLinks ctrl)
           obs
           (State (sws ++ sw' :: sws0) links ofLinks ctrl)
  | Step_TopoStep : forall sws sws0 sw sw' link link' links links0 ofLinks ctrl,
      TopoStep sw link sw' link' ->
      step (State (sws ++ sw :: sws0) (links ++ link :: links0) ofLinks ctrl)
           None
           (State (sws ++ sw' :: sws0) (links ++ link' :: links0) ofLinks ctrl)
  | Step_controller : forall sws links ofLinks ctrl ctrl',
      controller_step ctrl ctrl' ->
      step (State sws links ofLinks ctrl)
           None
           (State sws links ofLinks ctrl')
  | Step_ControllerOpenFlowStep : forall sws links ofLink ofLink'
      ofLinks ofLinks0 ctrl ctrl',
      ControllerOpenFlowStep ctrl ofLink ctrl' ofLink' ->
      step (State sws links (ofLinks ++ ofLink :: ofLinks0) ctrl)
           None
           (State sws links (ofLinks ++ ofLink' :: ofLinks0) ctrl')
  | Step_SwitchOpenFlowStep : forall sw sw' sws sws0 links ofLink ofLink'
      ofLinks ofLinks0 ctrl,
      SwitchOpenFlowStep sw ofLink sw' ofLink' ->
      step (State (sws ++ sw :: sws0) links (ofLinks ++ ofLink :: ofLinks0) 
                  ctrl)
           None
           (State (sws ++ sw' :: sws0) links (ofLinks ++ ofLink' :: ofLinks0)
             ctrl).

End ConcreteSemantics.
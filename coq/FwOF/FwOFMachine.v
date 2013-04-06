Set Implicit Arguments.

Require Import Coq.Lists.List.
Require Import Bag.TotalOrder.
Require Import Bag.Bag2.
Require Import Common.Types.
Require Import FwOF.FwOFSignatures.

Local Open Scope list_scope.
Local Open Scope equiv_scope.
Local Open Scope bag_scope.

Module Make (Atoms_ : ATOMS) <: MACHINE.

  Module Atoms := Atoms_.
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

  (* TODO(arjun): fix *)
  Inductive switch_le : switch -> switch -> Prop :=
  | SwitchLe : forall sw1 sw2,
      switchId_le (swId sw1) (swId sw2) ->
      switch_le sw1 sw2.

  Axiom Instance TotalOrder_switch : TotalOrder switch_le.
  
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

End Make.

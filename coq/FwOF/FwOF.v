Set Implicit Arguments.

Require Import Coq.Lists.List.
Require Import Coq.Structures.Equalities.
Require Import Coq.Classes.Equivalence.
Require Import Coq.Classes.EquivDec.
Require Import Common.Types.
Require Import Bag.Bag.

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

  Include NETWORK_ATOMS.

  Axiom topo : switchId * portId -> option (switchId * portId).
  Axiom abst_func : switchId -> portId -> packet -> list (portId * packet).

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
  
  Existing Instances  Eq_switchId Eq_portId Eq_packet EqDec_switchId
     EqDec_portId EqDec_packet.

  Instance FromController_Eq : Eq fromController.
  Proof.
    split. apply fromController_eq_dec.
  Qed.

  Instance FromSwitch_Eq : Eq fromSwitch.
  Proof.
    split. apply fromSwitch_eq_dec.
  Qed.

  Instance FlowTable_Eq : Eq flowTable := {
    eqdec := flowTable_eq_dec
  }.

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

  Section Equivalences.

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

    Hint Constructors switch_equiv.

    Lemma switch_equiv_is_Equivalence : Equivalence switch_equiv.
    Proof with intros; eauto.
      split.
      unfold Reflexive...
      destruct x.
      apply SwitchEquiv; apply reflexivity.
      unfold Symmetric...
      inversion H.
      apply SwitchEquiv; apply symmetry...
      unfold Transitive...
      destruct x. destruct y. destruct z.
      inversion H.
      inversion H0.
      subst.
      apply SwitchEquiv; eapply transitivity...
    Qed.

    Instance switch_Equivalence : Equivalence switch_equiv.
    Proof.
      exact switch_equiv_is_Equivalence.
    Qed.

    Instance switch_eqdec : EqDec switch switch_equiv.
    Proof with subst.
      unfold EqDec.
      unfold complement.
      intros.
      destruct x.
      destruct y.
      destruct (eqdec swId0 swId1).
      2: right; intros; inversion H; subst; contradiction n; reflexivity.
      subst.
      destruct (eqdec pts0 pts1).
      2: right; intros; inversion H; subst; contradiction n; reflexivity.
      subst.
      destruct (eqdec tbl0 tbl1).
      2: right; intros; inversion H; subst; contradiction n; reflexivity.
      subst.
      destruct (equiv_dec inp0 inp1).
      2: right; intros; inversion H; subst; contradiction c.
      destruct (equiv_dec outp0 outp1).
      2: right; intros; inversion H; subst; contradiction c.
      destruct (equiv_dec ctrlm0 ctrlm1).
      2: right; intros; inversion H; subst; contradiction c.
      destruct (equiv_dec switchm0 switchm1).
      2: right; intros; inversion H; subst; contradiction c.
      left.
      apply SwitchEquiv; trivial.
    Qed.

    Inductive stateEquiv : state -> state -> Prop :=
    | StateEquiv : forall sws1 sws2 links ofLinks ctrl,
      sws1 === sws2 ->
      stateEquiv
        (State sws1 links ofLinks ctrl) 
        (State sws2 links ofLinks ctrl).

    Hint Constructors stateEquiv.

    Lemma stateEquiv_is_Equivalence : Equivalence stateEquiv.
    Proof with eauto.
      split.
      unfold Reflexive.
      intros. destruct x. apply StateEquiv. apply reflexivity.
      unfold Symmetric.
      intros. destruct x; destruct y. inversion H. subst. apply StateEquiv.
      apply symmetry...
      unfold Transitive.
      intros. destruct x. destruct y. destruct z. inversion H. inversion H0.
      subst. apply StateEquiv. eapply transitivity...
    Qed.

  End Equivalences.

  Existing Instances switch_Equivalence switch_eqdec.

  Instance stateEquiv_Equivalence : Equivalence stateEquiv.
  Proof. 
    exact stateEquiv_is_Equivalence.
  Qed.
    
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
    | SafeWire_nil : forall swId tbl,
      FlowTableSafe swId tbl ->
      SafeWire swId (Endpoint_NoBarrier tbl) nil (Endpoint_NoBarrier tbl)
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

  End FlowModSafety.

End ConcreteSemantics.

Module Type ATOMS_AND_CONTROLLER.

  Module Atoms : ATOMS.
    Include ATOMS.
  End Atoms.

  Export Atoms.

  Require Import Common.Bisimulation.

  Module Export RelationDefs := ConcreteSemantics (Atoms).

  Axiom relate_controller : controller -> bag (switchId * portId * packet).

  Axiom ControllerRemembersPackets :
    forall (ctrl ctrl' : controller),
      controller_step ctrl ctrl' ->
      relate_controller ctrl = relate_controller ctrl'.

  Axiom ControllerSendForgetsPackets : forall ctrl ctrl' sw msg,
    controller_send ctrl ctrl' sw msg ->
    relate_controller ctrl === select_packet_out sw msg <+>
    relate_controller ctrl'.

  Axiom ControllerRecvRemembersPackets : forall ctrl ctrl' sw msg,
    controller_recv ctrl sw msg ctrl' ->
    relate_controller ctrl' === select_packet_in sw msg <+> 
    (relate_controller ctrl).

  Axiom ControllerLiveness : forall sw pt pk ctrl0 sws0 links0 ofLinks0,
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

  Axiom ControllerFMS : forall swId ctrl0 ctrl1 ctrlEp0 switchEp msg ctrlm
    switchm sws links ofLinks0 ofLinks1,
    SafeWire swId ctrlEp0 ctrlm switchEp ->
    step
      (State
        sws links
        (ofLinks0 ++ (OpenFlowLink swId switchm ctrlm) :: ofLinks1)
        ctrl0)
      None
      (State
        sws links
        (ofLinks0 ++ (OpenFlowLink swId switchm (msg :: ctrlm)) :: ofLinks1)
        ctrl1) ->
      exists ctrlEp1,
        SafeWire swId ctrlEp1 (msg :: ctrlm) switchEp.

End ATOMS_AND_CONTROLLER.

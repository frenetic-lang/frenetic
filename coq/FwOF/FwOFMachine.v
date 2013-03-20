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

  Instance TotalOrder_switch : TotalOrder switch_le.
  Proof with auto.
    split.
  Admitted.
  
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
      (forall msg, In msg (to_list ctrlm) -> NotFlowMod msg) ->
      table_at_endpoint swEp = tbl ->
      SwitchEP
        (Switch swId pts tbl inp outp ctrlm switchm)
        swEp
    | OneFlowModInBuffer : forall swId pts tbl inp outp ctrlm ctrlm0 switchm f,
      (forall msg, In msg (to_list ctrlm0) -> NotFlowMod msg) ->
      ctrlm = ({|FlowMod f|} <+> ctrlm0) ->
      FlowTableSafe swId (modify_flow_table f tbl) ->
      SwitchEP (Switch swId pts tbl inp outp ctrlm switchm)
               (Endpoint_NoBarrier (modify_flow_table f tbl)).

    (** "FMS" is short for "flow mod safety". *)
    Inductive FMS : switch -> openFlowLink -> Prop := 
    | MkFMS : forall swId pts tbl inp outp ctrlm switchm ctrlmList 
                     switchmList
                     switchEp,
      SwitchEP (Switch swId pts tbl inp outp ctrlm switchm) switchEp ->
      (exists ctrlEp, SafeWire swId ctrlEp ctrlmList switchEp) ->
      FMS 
        (Switch swId pts tbl inp outp ctrlm switchm)
        (OpenFlowLink swId switchmList ctrlmList).

    Definition AllFMS (sws : bag switch_le) 
                      (ofLinks : list openFlowLink) :=
      forall sw,
        In sw (to_list sws) ->
        exists lnk, 
          In lnk ofLinks /\
          of_to lnk = swId sw /\
          FMS sw lnk.


    Hint Constructors Endpoint SafeWire NotFlowMod SwitchEP FMS.


    Lemma FMS_untouched : forall swId0 pts0 tbl0 inp0 outp0 ctrlm0 switchm0
      switchmLst0 ctrlmLst0 inp1 outp1 switchm1 switchmLst1,
      FMS (Switch swId0 pts0 tbl0 inp0 outp0 ctrlm0 switchm0)
          (OpenFlowLink swId0 switchmLst0 ctrlmLst0) ->
      FMS (Switch swId0 pts0 tbl0 inp1 outp1 ctrlm0 switchm1)
          (OpenFlowLink swId0 switchmLst1 ctrlmLst0).
    Proof.
      intros.
      inversion H; inversion H2; subst; eauto.
    Qed.


    Lemma SafeWire_dequeue_PacketOut : forall sw ctrlEp ctrlLst pt pk switchEp,
      SafeWire sw ctrlEp (ctrlLst ++ [PacketOut pt pk]) switchEp ->
      SafeWire sw ctrlEp (ctrlLst) switchEp.
    Proof with auto.
      intros.
      generalize dependent ctrlEp.
      induction ctrlLst; intros.
      + simpl in H. inversion H. subst...
      + inversion H; subst...
    Qed.

    Lemma SafeWire_dequeue_BarrierRequest : forall sw ctrlEp ctrlLst xid 
      switchEp1,
      SafeWire sw ctrlEp (ctrlLst ++ [BarrierRequest xid]) switchEp1 ->
      exists switchEp2, SafeWire sw ctrlEp (ctrlLst) switchEp2 /\
        table_at_endpoint switchEp2 = table_at_endpoint switchEp1.
    Proof with eauto.
      intros.
      generalize dependent ctrlEp.
      induction ctrlLst; intros.
      + inversion H; subst.
        inversion H5. subst.
        exists (Endpoint_Barrier (table_at_endpoint switchEp1)).
        split.
        apply SafeWire_nil.
        simpl...
        simpl...
      + simpl in H.
        inversion H; subst.
        * apply IHctrlLst in H5.
          destruct H5 as [switchEp2 [HSafeWire HEpEq]].
          exists switchEp2.
          split; simpl...
        * apply IHctrlLst in H5.
          destruct H5 as [switchEp2 [HSafeWire HEpEq]].
          exists switchEp2.
          split; simpl...
        * apply IHctrlLst in H6.
          destruct H6 as [switchEp2 [HSafeWire HEpEq]].
          exists switchEp2.
          split; simpl...
    Qed.

    Lemma SafeWire_dequeue_safe : forall sw ctrlEp ctrlLst f switchEp,
       SafeWire sw ctrlEp (ctrlLst ++ [FlowMod f]) switchEp ->
       FlowTableSafe sw (modify_flow_table f (table_at_endpoint switchEp)).
    Proof with auto.
      intros.
      generalize dependent ctrlEp.
      { induction ctrlLst; intros.
        + destruct switchEp.
          simpl in H.
          inversion H.
          subst.
          inversion H6.
          simpl in H.
          inversion H.
          subst.
          inversion H6.
          subst.
          simpl...
        + simpl in *.
          inversion H; subst.
          - apply IHctrlLst in H5...
          - apply IHctrlLst in H5...
          - apply IHctrlLst in H6... }
    Qed.

    Lemma SafeWire_dequeue_FlowMod : forall sw ctrlEp ctrlLst f switchEp,
       SafeWire sw ctrlEp (ctrlLst ++ [FlowMod f]) switchEp ->
       SafeWire sw ctrlEp ctrlLst (Endpoint_NoBarrier (modify_flow_table f (table_at_endpoint switchEp))).
    Proof with auto.
      intros.
      generalize dependent ctrlEp.
      { induction ctrlLst; intros.
        + destruct switchEp.
          simpl in H.
          inversion H.
          subst.
          inversion H6.
          simpl in H.
          inversion H.
          subst.
          inversion H6.
          subst.
          simpl...
        + simpl in *.
          inversion H; subst.
          - apply IHctrlLst in H5...
          - apply IHctrlLst in H5...
          - apply IHctrlLst in H6... }
    Qed.

    Lemma SafeWire_fm_nb_false : forall sw ctrlEp ctrlLst f tbl,
      SafeWire sw ctrlEp (ctrlLst ++ [FlowMod f]) (Endpoint_NoBarrier tbl) ->
      False.
    Proof with auto.
      intros.
      generalize dependent ctrlEp.
      induction ctrlLst; intros.
      + simpl in H. inversion H. subst. inversion H6.
      + simpl in H.
        inversion H; subst.
        - apply IHctrlLst in H5...
          - apply IHctrlLst in H5...
          - apply IHctrlLst in H6...
    Qed.

    Lemma FMS_pop : forall sw pts tbl inp outp ctrlm switchm switchLst ctrlLst msg,
      FMS  (Switch sw pts tbl inp outp ctrlm switchm) (OpenFlowLink sw switchLst (ctrlLst ++ [msg])) ->
      FMS  (Switch sw pts tbl inp outp ({|msg|} <+> ctrlm) switchm) (OpenFlowLink sw switchLst ctrlLst).
    Proof with eauto.
      intros.
      inversion H.
      subst.
      destruct H10 as [ctrlEp HSafeWire].
      { destruct msg.
        + apply MkFMS with (switchEp := switchEp).
          * inversion H2; subst.
            - apply NoFlowModsInBuffer...
              intros.
              apply Bag.in_union in H0.
              simpl in H0.
              destruct H0 as [[H0 | H0] | H0]...
              destruct msg...
              inversion H0.
              inversion H0.
            - apply OneFlowModInBuffer 
                with (ctrlm0 := ({|PacketOut p p0|} <+> ctrlm2))...
              intros.
              apply Bag.in_union in H0.
              simpl in H0.
              destruct H0 as [[H0 | H0] | H0]...
              destruct msg...
              inversion H0.
              inversion H0.
              bag_perm 10.
          * exists ctrlEp.
            eapply SafeWire_dequeue_PacketOut.
            exact HSafeWire.
        + admit. (* strange case where barrier is added to switch. *)
        + inversion H2; subst.
          * apply MkFMS with (switchEp := (Endpoint_NoBarrier (modify_flow_table f (table_at_endpoint switchEp)))).
            apply OneFlowModInBuffer with (ctrlm0 := ctrlm0)...
            eapply SafeWire_dequeue_safe.
            exact HSafeWire.
            exists ctrlEp.
            apply SafeWire_dequeue_FlowMod...
         * contradiction (SafeWire_fm_nb_false _ _ HSafeWire).
      }
    Qed.

    Lemma FMS_dequeue_pktOut : forall sw pts tbl inp outp ctrlm switchm switchLst ctrlLst pt pk,
      FMS  (Switch sw pts tbl inp outp ({|PacketOut pt pk|} <+> ctrlm) switchm) (OpenFlowLink sw switchLst ctrlLst) ->
      FMS  (Switch sw pts tbl inp ({|(pt,pk)|} <+> outp) ctrlm switchm) (OpenFlowLink sw switchLst ctrlLst).
    Proof with eauto.
      intros.
      inversion H.
      subst.
      destruct H10 as [ctrlEp HSafeWire].
      apply MkFMS with (switchEp := switchEp).
      inversion H2; subst.
      - apply NoFlowModsInBuffer...
        intros.
        apply H9.
        apply Bag.in_union...
      - assert (In (PacketOut pt pk) (to_list ctrlm2)) as J.
        {      
          assert (In (PacketOut pt pk) (to_list ({|FlowMod f|} <+> ctrlm2))).
          { rewrite <- H10. apply Bag.in_union. left. simpl... }
          apply Bag.in_union in H0.
          destruct H0 as [[H0 | H0] | H0]...
          inversion H0.
          inversion H0. } 
        apply Bag.in_split with (Order:=TotalOrder_fromController) in J.
        destruct J as [ctrlm1 Heq].
        subst.
        apply OneFlowModInBuffer with
          (ctrlm0 := ctrlm1)...
        intros.
        apply H9.
        apply Bag.in_union. right...
        apply (Bag.pop_union_l _ ({|PacketOut pt pk|})).
        rewrite -> H10.
        bag_perm 100. 
      - exists ctrlEp...
    Qed.

  End FlowModSafety.

End Make.

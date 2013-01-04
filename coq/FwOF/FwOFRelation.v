Set Implicit Arguments.

Require Import Coq.Lists.List.
Require Import Coq.Classes.Equivalence.
Require Import Coq.Structures.Equalities.
Require Import Coq.Classes.Morphisms.
Require Import Coq.Setoids.Setoid.
Require Import Common.Types.
Require Import Common.Bisimulation.
Require Import Bag.Bag.
Require Import FwOF.FwOF.

Local Open Scope list_scope.
Local Open Scope equiv_scope.
Local Open Scope bag_scope.

Module Make (Import Atoms : ATOMS).

  Module Concrete := ConcreteSemantics (Atoms).
  Import Concrete.

  Axiom topo : switchId * portId -> option (switchId * portId).

  Definition abst_state := Bag.bag (switchId * portId * packet).

  Axiom relate_controller : controller -> abst_state.

  Axiom abst_func : switchId -> portId -> packet -> list (portId * packet).

  Definition affixSwitch (sw : switchId) (ptpk : portId * packet) :=
    match ptpk with
      | (pt,pk) => (sw,pt,pk)
    end.

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

  Axiom locate_packet_in : switchId -> portId -> packet -> 
    list (portId * packet).

  Definition select_packet_in (sw : switchId) (msg : fromSwitch) :=
    match msg with
      | PacketIn pt pk => 
        Bag.FromList (map (affixSwitch sw) (locate_packet_in sw pt pk))
      | _ => {| |}
    end.

  Definition flow_table_safe (sw : switchId) (tbl : flowTable) : Prop :=
    forall pt pk forwardedPkts packetIns,
      process_packet tbl pt pk = (forwardedPkts, packetIns) ->
      Bag.unions (map (transfer sw) forwardedPkts) <+>
      Bag.unions (map (select_packet_in sw) (map (PacketIn pt) packetIns)) ===
      Bag.unions (map (transfer sw) (abst_func sw pt pk)).

  Definition ConsistentDataLinks (st : state) : Prop :=
    forall (lnk : dataLink),
      In lnk (state_dataLinks st) ->
      topo (dataLink_src lnk) = Some (dataLink_dst lnk).

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
    
  Record concreteState := ConcreteState {
    concreteState_state : state;
    concreteState_flowTableSafety :
      forall (sw : switch), 
        In sw (state_switches concreteState_state) -> 
        flow_table_safe (switch_swichId sw) (switch_flowTable sw);
    concreteState_consistentDataLinks :
      ConsistentDataLinks concreteState_state
  }.

  Definition concreteStep (st : concreteState) (obs : option observation)
    (st0 : concreteState) :=
    step (concreteState_state st) obs (concreteState_state st0).

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
        Bag.FromList (map (affixSwitch swId) (Bag.to_list inp)) <+>
        Bag.unions (map (transfer swId) (Bag.to_list outp)) <+>
        Bag.unions (map (select_packet_out swId) (Bag.to_list ctrlm)) <+>
        Bag.unions (map (select_packet_in swId) (Bag.to_list switchm))
    end.

  Definition relate_dataLink (link : dataLink) : abst_state :=
    match link with
      | DataLink _ pks (sw,pt) =>
        Bag.FromList (map (fun pk => (sw,pt,pk)) pks)
    end.

  Definition relate_openFlowLink (link : openFlowLink) : abst_state :=
    match link with
      | OpenFlowLink sw switchm ctrlm =>
        Bag.unions (map (select_packet_out sw) ctrlm) <+>
        Bag.unions (map (select_packet_in sw) switchm)
    end.


  Definition relate (st : state) : abst_state :=
    Bag.unions (map relate_switch (state_switches st)) <+>
    Bag.unions (map relate_dataLink (state_dataLinks st)) <+>
    Bag.unions (map relate_openFlowLink (state_openFlowLinks st)) <+>
    relate_controller (state_controller st).

  Definition bisim_relation : relation concreteState abst_state :=
    fun (st : concreteState) (ast : abst_state) => 
      ast === (relate (concreteState_state st)).

  Theorem weak_sim_2 :
    weak_simulation abstractStep concreteStep (inverse_relation bisim_relation).
  Proof with auto.
    unfold weak_simulation.
    intros.
    unfold inverse_relation in H.
    unfold bisim_relation in H.
    unfold relate in H.
    destruct t. simpl in *.
    split; intros.
    (* Observation steps. *)
    inversion H0; subst.
    destruct concreteState_state0.
    simpl in *.
    assert (Bag.Mem (sw,pt,pk) (({|(sw,pt,pk)|}) <+> lps)) as J.
      simpl...
    remember (Bag.Mem_equiv (sw,pt,pk) H J) as X.
    clear HeqX.
    simpl in X.
    destruct X as [X | [X | X]].
    (* The packet in on a switch. *)
    induction state_switches0.
    simpl in X. inversion X.
    simpl in X. destruct X as [X | X].
    destruct a.
    simpl in X.
    destruct X as [X | [X | X]].
    (* The packet is in the input buffer *)
    induction switch_inputPackets0.
    simpl in X. inversion X.
    simpl in X.
    destruct X as [X | X]; (idtac || inversion X).
    destruct a.
    simpl in X.
    inversion X.
    subst.
  Admitted.

  Theorem weak_sim_1 :
    weak_simulation concreteStep abstractStep bisim_relation.
  Proof with auto with datatypes.
    unfold weak_simulation.
    intros.
    unfold bisim_relation in H.
    unfold relate in H.
    destruct s. simpl in *.
    split; intros.
    unfold concreteStep in H0.
    destruct s'. simpl in *.
    inversion H0; subst.
    inversion H1; subst.
    simpl in *.
    autorewrite with bag in H using (simpl in H)...
    (* Pick out the observation. *)
    rewrite -> Bag.union_comm in H.
    rewrite -> Bag.union_assoc in H.
    match goal with
      | [ H : t === ?t0 <+> ?t1 |- _ ] => remember t1
    end.
    exists (Bag.unions (map (transfer swId) (abst_func swId pt pk)) <+> b).
    split.
    (* Showing the states are bisimilar *)
    unfold bisim_relation.
    unfold relate.
    simpl.
    rewrite -> map_app.
    rewrite -> Bag.unions_app.
    simpl.
    rewrite -> Bag.unions_unlist_2.
    rewrite -> Bag.unions_unlist_2.
    repeat rewrite -> Bag.union_assoc.
    assert (flow_table_safe swId tbl) as J.
      refine (concreteState_flowTableSafety1
        (Switch swId pts tbl inp (Bag.FromList outp' <+> outp) ctrlm
          (Bag.FromList (map (PacketIn pt) pksToCtrl) <+> switchm)) _)...
    unfold flow_table_safe in J.
    pose (J0 := J pt pk outp' pksToCtrl H3).
    subst.
    rewrite <- J0.
    repeat rewrite -> Bag.union_assoc.
    bag_perm 100. (* #winning *)
    (* STUPID TYPE CLASSES WHY *)
    admit.
    admit.
    apply multistep_tau with (a0 := ({|(swId, pt, pk)|}) <+> b).
    apply AbstractStepEquiv...
    apply multistep_obs with
      (a0 := (Bag.unions (map (transfer swId) (abst_func swId pt pk)) <+> b)).
    apply AbstractStep.
    subst.
    apply multistep_nil.
    (* steps with no observations. *)
    intros.
    unfold bisim_relation in H.
    destruct s'.
    inversion H0; subst; simpl in *.
    (* Switch steps independently *)
    inversion H1; subst; simpl in H.
    (* Switch processes a flow-mod *)
    autorewrite with bag in H using (simpl in H)...
    exists t.
    split.
    unfold bisim_relation.
    unfold relate.
    simpl.
    autorewrite with bag using simpl.
    exact H.
    apply multistep_nil.
    (* Switch sends a PacketOut message out. *)
    autorewrite with bag in H using (simpl in H)...
    exists t.
    split.
    unfold bisim_relation.
    unfold relate.
    simpl.
    autorewrite with bag using simpl.
    rewrite -> H.
    bag_perm 100. (* #winning *)
    apply multistep_nil.
    (* Switch sends/receives packets on the network. *)
    inversion H1; subst; simpl in H.
    (* Switch sends a packet out. *)
    autorewrite with bag in H using (simpl in H)...
    exists t.
    split.
    unfold bisim_relation.
    unfold relate.
    simpl.
    destruct dst.
    autorewrite with bag using simpl.
    rewrite -> H.
    clear H.
    clear H0.
    clear concreteState_flowTableSafety1.
    clear concreteState_flowTableSafety0.
    unfold ConsistentDataLinks in concreteState_consistentDataLinks0.
    rename concreteState_consistentDataLinks0 into H.
    simpl in H.
    assert (In (DataLink (swId,pt) pks (s,p)) 
               (links ++ (DataLink (swId,pt) pks (s,p))  :: links0)) as J...
    apply H in J.
    simpl in J.
    rewrite -> J.
    bag_perm 100. (* #winning *)
    apply multistep_nil.
    (* Switch reads a packet off a data-link *)
    autorewrite with bag in H using (simpl in H)...
    exists t.
    split.
    unfold bisim_relation.
    unfold relate.
    simpl.
    rewrite -> H.
    autorewrite with bag using simpl.
    bag_perm 100. (* #winning *)
    apply multistep_nil.
    (* Controller takes an internal step. *)
    subst.
    simpl in *.
    exists t.
    split.
    unfold bisim_relation.
    unfold relate.
    rewrite -> H.
    simpl.
    rewrite -> (ControllerRemembersPackets H1).
    apply reflexivity.
    apply multistep_nil.
    (* Controller sends/recvs an OpenFlow message. *)
    inversion H1; subst; simpl in H.
    (* Controller recvs an OpenFlow message. *)
    autorewrite with bag in H using (simpl in H)...
    (* TODO(arjun): why didn't autorewrite do this? *)
    rewrite -> (Bag.unions_app _ (map relate_openFlowLink ofLinks)) in H.
    autorewrite with bag in H using (simpl in H)...
    exists t.
    split.
    unfold bisim_relation.
    unfold relate.
    simpl.
    autorewrite with bag using simpl.
    rewrite -> H.
    rewrite -> (ControllerRecvRemembersPackets H3).
    bag_perm 100. (* #winning *)
    apply multistep_nil.
    (* Controller sends an OpenFlow message. *)
    exists t.
    split.
    unfold bisim_relation.
    unfold relate.
    rewrite -> H.
    simpl.
    autorewrite with bag using simpl.
    rewrite -> (ControllerSendForgetsPackets H3).
    bag_perm 100. (* #winning *)
    apply multistep_nil.
    (* Switch sends/recvs an OpenFlow message!!! *)
    inversion H1; subst; simpl in H.
    (* Switch sends an OpenFlow message. *)
    exists t.
    split.
    unfold bisim_relation.
    unfold relate.
    rewrite -> H.
    autorewrite with bag using simpl.
    bag_perm 100. (* #winning *)
    apply multistep_nil.
    (* Switch responds to a barrier. *)
    exists t.
    split.
    unfold bisim_relation.
    unfold relate.
    rewrite -> H.
    autorewrite with bag using simpl.
    do 2 (rewrite -> (Bag.unions_app _ (map relate_switch sws))).
    do 2 (rewrite -> (Bag.unions_app _ (map relate_openFlowLink ofLinks))).
    autorewrite with bag using simpl.
    bag_perm 100. (* #winning *)
    apply multistep_nil.
    (* Switch responds to a non-barrier message. *)
    exists t.
    split.
    unfold bisim_relation.
    unfold relate.
    rewrite -> H.
    autorewrite with bag using simpl.
    do 2 (rewrite -> (Bag.unions_app _ (map relate_switch sws))).
    do 2 (rewrite -> (Bag.unions_app _ (map relate_openFlowLink ofLinks))).
    autorewrite with bag using simpl.
    bag_perm 100. (* #winning *)
    apply multistep_nil.
  Qed.


  Theorem fwof_abst_weak_bisim :
    weak_bisimulation concreteStep abstractStep bisim_relation.
  Proof.
    unfold weak_bisimulation.
    split.
    exact weak_sim_1.
    exact weak_sim_2.
  Qed.

End Make.    
  

  

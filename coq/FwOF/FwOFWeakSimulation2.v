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
Require FwOF.FwOFRelation.

Local Open Scope list_scope.
Local Open Scope equiv_scope.
Local Open Scope bag_scope.

Module Make (Import Atoms : ATOMS).

  Module Relation := FwOF.FwOFRelation.Make (Atoms).
  Import Relation.
  Import Relation.Concrete.


  Axiom Mem_unions : forall (A : Type) (E : Eq A) (x : A) lst, 
    Bag.Mem x (Bag.unions lst) ->
    exists elt, In elt lst /\ Bag.Mem x elt.

  Lemma FlowTablesSafe_untouched : forall 
    {swId tbl pts 
    sws sws0 links ofLinks ctrl
    inp outp ctrlm switchm
    inp' outp' ctrlm' switchm'},
    FlowTablesSafe
      (State (sws ++ (Switch swId pts tbl inp outp ctrlm switchm) :: sws0)
             links ofLinks ctrl) ->
    FlowTablesSafe
      (State (sws ++ (Switch swId pts tbl inp' outp' ctrlm' switchm') :: sws0)
             links ofLinks ctrl).
  Proof with auto with datatypes.
    unfold FlowTablesSafe in *.
    intros.
    apply in_app_iff in H0.
    simpl in H0.
    simpl in H.
    destruct H0 as [HIn | [HIn | HIn]]...
    rewrite <- HIn.
    simpl.
    pose (sw0 := Switch swId pts tbl inp outp ctrlm switchm).
    assert (In sw0 (sws ++ sw0 :: sws0)) as X...
    apply H in X.
    simpl in X...
  Qed.

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
    clear J.
    destruct X as [X | [X | X]].
    (* The packet in on a switch. *)
    apply Mem_unions in X.
    destruct X as [switch_abst [ Xin Xmem ]].
    rewrite -> in_map_iff in Xin.
    destruct Xin as [switch [Xrel Xin]].
    subst.
    destruct switch.
    simpl in Xmem.
    destruct Xmem as [Xmem | [Xmem | Xmem]].
    (* The packet is in the input buffer. *)
    (* Need to get switch_inputPackets = l1 ++ (pt,pk) :: l2 *)
    rewrite -> in_map_iff in Xmem.
    destruct Xmem as [ ptpk [XmemEq Xmem]].
    apply in_split in Xmem.
    destruct Xmem as [pks [pks0 Xmem]].
    apply in_split in Xin.
    destruct Xin as [sws [sws0 Xin]].
    remember (process_packet switch_flowTable0 pt pk) as Y.
    destruct Y as [ newPks pktIns ].
    pose (state0 :=
      (State 
        (sws ++ 
          (Switch switch_swichId0 switch_ports0 switch_flowTable0
            ((Bag.FromList pks) <+> (Bag.FromList pks0))
            (Bag.FromList newPks <+> switch_outputPackets0)
            switch_fromController0
            (Bag.FromList (map (PacketIn pt)  pktIns) <+> switch_fromSwitch0))
          :: sws0)
        state_dataLinks0
        state_openFlowLinks0
        state_controller0)).
    subst.
    exists
      (@ConcreteState
        state0
        (FlowTablesSafe_untouched concreteState_flowTableSafety0)
        concreteState_consistentDataLinks0).
    simpl in *.
    split.
    unfold inverse_relation.
    unfold bisim_relation.
    unfold relate.
    simpl.

    Axiom unpop_unions : forall (A : Type) (E : Eq A) (b b0 b1 : Bag.bag A),
      Bag.Union b b0 === Bag.Union b b1 ->
      b0 === b1.

    clear concreteState_consistentDataLinks0.
    pose (switch0 := Switch switch_swichId0 switch_ports0 switch_flowTable0 
      switch_inputPackets0
      switch_outputPackets0
      switch_fromController0
      switch_fromSwitch0).
    
    assert (FlowTableSafe (switch_swichId switch0) 
               (switch_flowTable switch0)) as Z.
      apply concreteState_flowTableSafety0.
      simpl. auto with datatypes.

    clear concreteState_flowTableSafety0.

    unfold FlowTableSafe in Z.

    apply unpop_unions with (b := ({|(sw,pt,pk)|})).
    rewrite -> Bag.union_comm.
    rewrite -> Bag.union_assoc.
    rewrite -> (Bag.union_comm _ lps).
    rewrite -> H.
    clear H.

    rewrite -> map_app.
    rewrite -> (Bag.unions_app _ (map relate_switch sws)).
    autorewrite with bag using simpl.
    rewrite -> (Bag.unions_app _ (map relate_switch sws)).
    autorewrite with bag using simpl.
    rewrite -> Xmem.
    rewrite -> map_app.
    autorewrite with bag using simpl.
    rewrite -> XmemEq.
    symmetry in HeqY.
    apply Z in HeqY.
    simpl in HeqY.
    assert (switch_swichId0 = sw) as W.
      destruct ptpk.
      simpl in XmemEq.
      inversion XmemEq...
    clear Z.
    subst.
    rewrite <- HeqY.
    autorewrite with bag using simpl.
    bag_perm 100.
    apply multistep_obs with 
      (a0 := (@ConcreteState
        state0
        (FlowTablesSafe_untouched concreteState_flowTableSafety0)
        concreteState_consistentDataLinks0)).
    unfold concreteStep.
    simpl in *.

    
  Admitted.

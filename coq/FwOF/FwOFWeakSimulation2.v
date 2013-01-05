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

  Axiom unpop_unions : forall (A : Type) (E : Eq A) (b b0 b1 : Bag.bag A),
    Bag.Union b b0 === Bag.Union b b1 ->
    b0 === b1.

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
    intros.
  Admitted.

  Lemma ConsistentDataLinks_untouched : forall 
    { sws sws' links ofLinks ofLinks' ctrl ctrl' },
    ConsistentDataLinks (State sws links ofLinks ctrl) ->
    ConsistentDataLinks (State sws' links ofLinks' ctrl').
  Proof with auto with datatypes.
    intros.
    unfold ConsistentDataLinks in *.
    intros.
    simpl in *...
  Qed.

  Theorem weak_sim_2 :
    weak_simulation abstractStep concreteStep (inverse_relation bisim_relation).
  Proof with simpl;auto.
    unfold weak_simulation.
    intros.
    unfold inverse_relation in H.
    unfold bisim_relation in H.
    unfold relate in H.
    destruct t.
    simpl in *.
    inversion H0; subst.
    (* Idiotic case, where the two abstract states are equivalent. *)
    admit.
    (* Real cases here. *)
    simpl.
    destruct concreteState_state0.
(*    remember (State state_switches0 state_dataLinks0 state_openFlowLinks0
                    state_controller0) as state0. *)
    (* The first challenge is to figure out what the cases are. We cannot
       proceed by inversion or induction. Instead, hypothesis H entails
       that (sw,pt,pk) is in one of the concrete components. Bag.Mem defines
       how this may be. *)
    assert (Bag.Mem (sw,pt,pk) (({|(sw,pt,pk)|}) <+> lps)) as J...
    (* By J, (sw,pt,pk) is in the left-hand side of H. By Mem_equiv,
       (sw,pt,pk) is also on the right-hand side too. *)
    destruct (Bag.Mem_equiv (sw,pt,pk) H J) as 
      [HMemSwitch | [ HMemLink | [HMemOFLink | HMemCtrl ]]].
    (* The packet in on a switch. *)
    apply Mem_unions in HMemSwitch.
    destruct HMemSwitch as [switch_abst [ Xin Xmem ]].
    rewrite -> in_map_iff in Xin.
    destruct Xin as [switch [Xrel Xin]].
    apply in_split in Xin.
    destruct Xin as [sws [sws0 Xin]].
    simpl in Xin.
    subst.
    destruct switch.
    simpl in Xmem.
    simpl in H.
    autorewrite with bag in H using (simpl in H).
    destruct Xmem as [HMemInp | [HMemOutp | [HMemCtrlm | HMemSwitchm]]].

    (* At this point, we've discriminated all but one other major case.
       Packets on OpenFlow links may be on either side. *)

    assert (switch_swichId0 = sw) as J0.
      rewrite -> in_map_iff in HMemInp.
      destruct HMemInp as [[pt0 pk0] [Haffix HMemInp]].
      simpl in Haffix.
      inversion Haffix...
    subst.

    rewrite -> in_map_iff in HMemInp.
    destruct HMemInp as [[pt0 pk0] [Haffix HMemInp]].
    simpl in Haffix.
    inversion Haffix.
    subst.
    assert (exists x, switch_inputPackets0 === ({|(pt,pk)|} <+> x)). admit.
    destruct H1 as [inp HEqInp].

    remember (process_packet switch_flowTable0 pt pk) as ToCtrl eqn:Hprocess. 
    destruct ToCtrl as [outp' packetIns].
    symmetry in Hprocess.    

    remember (PktProcess sw switch_ports0 
                 inp  switch_outputPackets0
                switch_fromController0 switch_fromSwitch0 
                Hprocess
                sws sws0 state_dataLinks0 state_openFlowLinks0
                state_controller0) as Hstep.
    clear HeqHstep.
    match goal with
      | [ H : step _ _ ?S |- _  ] => 
        exists 
          (ConcreteState S
            (FlowTablesSafe_untouched concreteState_flowTableSafety0)
            (ConsistentDataLinks_untouched concreteState_consistentDataLinks0))
    end.
    split.
    unfold inverse_relation.
    unfold bisim_relation.
    unfold relate.
    simpl.
    rewrite -> map_app.
    autorewrite with bag using simpl.

    assert (FlowTableSafe sw switch_flowTable0) as Z.
      unfold FlowTablesSafe in concreteState_flowTableSafety0.
      apply concreteState_flowTableSafety0 with 
        (pts := switch_ports0)
        (inp := switch_inputPackets0)
        (outp := switch_outputPackets0)
        (ctrlm := switch_fromController0)
        (switchm := switch_fromSwitch0).
      simpl.
      auto with datatypes.
    unfold FlowTableSafe in Z.
    rewrite <- (Z pt pk outp' packetIns Hprocess).
    clear Z.

    apply unpop_unions with (b := ({|(sw,pt,pk)|})).
    rewrite -> Bag.union_comm.
    rewrite -> Bag.union_assoc.
    rewrite -> (Bag.union_comm _ lps).
    rewrite -> H.

    rewrite -> (Bag.FromList_map_iff _ _ HEqInp).
    rewrite -> Bag.map_union.

    Axiom to_list_singleton : forall (A : Type) (E : Eq A) (x : A),
      Bag.to_list (Bag.Singleton x) === [x].

    Axiom from_list_singleton : forall (A : Type) (E : Eq A) (x : A),
      Bag.FromList [x] === Bag.Singleton x.

    rewrite -> to_list_singleton.
    simpl.
    rewrite -> from_list_singleton.

    repeat rewrite -> Bag.union_assoc.

    bag_perm 100.
    admit. (* TYPE CLASSES *)
    admit.
    match goal with
      | [ H : step ?st _ _ |- _ ] =>
        apply multistep_tau with 
          (a0 := ConcreteState st
            (FlowTablesSafe_untouched concreteState_flowTableSafety0)
            (ConsistentDataLinks_untouched concreteState_consistentDataLinks0))
    end.
    apply StepEquivSwitch.
      unfold Equivalence.equiv.
      apply SwitchEquiv; try solve [ apply reflexivity | auto ].
    match goal with
      | [ H : step _ _ ?st |- _ ] =>
        apply multistep_obs with 
          (a0 := ConcreteState st
            (FlowTablesSafe_untouched concreteState_flowTableSafety0)
            (ConsistentDataLinks_untouched concreteState_consistentDataLinks0))
    end.
    exact Hstep.
    apply multistep_nil.

    (* Case where packet is in the output buffer. *)


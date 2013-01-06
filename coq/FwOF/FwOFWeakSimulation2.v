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
Require Import Coq.Logic.ProofIrrelevance.

Local Open Scope list_scope.
Local Open Scope equiv_scope.
Local Open Scope bag_scope.

Module Make (Import Atoms : ATOMS).

  Module Relation := FwOF.FwOFRelation.Make (Atoms).
  Import Relation.
  Import Relation.Concrete.

  Lemma DrainWire : DrainWire_tau.
  Proof.
    unfold DrainWire_tau.
    intros.
    induction pks0.
    exists inp0.
    exists hasSrc0.
    exists hasDst0.
    exists tblsOK.
    assert (ConsistentDataLinks
      (State (sws ++ (Switch swId0 pts0 tbl0 inp0 outp0 ctrlm0 switchm0) ::
              sws0)
             (MapEquivLoc links0 ++
               linkEquiv (DataLink _ src0 [pk] (swId0,pt) hasSrc0 hasDst0) ::
               MapEquivLoc links1)
             ofLinks0
             ctrl0)) as X. admit.
    exists X.
    unfold MapEquivLoc in *.
    simpl in X.
    assert (X = linkTopoOK).

    apply multistep_nil.
    rewrite -> EquivLocExists_idem in *.
    Check X.
    simpl.
    Check linkTopoOK.
    apply multistep_nil.
                              src := src0;
                              pks := [pk];
                              dst := (swId0, pt);
                              hasSrc := EquivLocExists hasSrc0;
                              hasDst := EquivLocExists hasDst0 |}
                              :: MapEquivLoc links1;
                     ofLinks := ofLinks0;
                     ctrl := ctrl0 |}
    exists linkTopoOK.

    (sws sws0 : list switch)
    (swId : switchId)
    (pts : list portId)
    (tbl : flowTable)
    (inp outp : Bag.bag (portId * packet))
    (ctrlm : Bag.bag fromController)
    (switchm : Bag.bag fromSwitch)
    (links links0 : list (dataLink
      (sws ++ 
        (Switch swId pts tbl inp outp ctrlm switchm) ::
        sws0)))
    (ctrl : controller)
    (ofLinks : list openFlowLink)
    (src : switchId * portId)
    (pt : portId)
    (pk : packet)
    (pks : list packet)
    (hasSrc0 : LocExists
      (sws ++ (Switch swId pts tbl inp outp ctrlm switchm) :: sws0)
      src)
    (hasDst0 : LocExists
      (sws ++ (Switch swId pts tbl inp outp ctrlm switchm) :: sws0)
      (swId,pt))
    tblsOK linkTopoOK,
    exists 
      inp' hasSrc' hasDst' tblsOK' linkTopoOK',
      multistep concreteStep
      (ConcreteState
        (State 
          (sws ++ (Switch swId pts tbl inp outp ctrlm switchm) :: sws0)
          (links ++ 
            (DataLink
              (sws ++ (Switch swId pts tbl inp outp ctrlm switchm) :: sws0)
              src (pk :: pks) (swId,pt) hasSrc0 hasDst0) :: 
            links0)
          ofLinks 
          ctrl)
        tblsOK linkTopoOK)
       nil
       (ConcreteState
         (State 
           (sws ++ (Switch swId pts tbl inp' outp ctrlm switchm):: sws0)
           (MapEquivLoc links ++ 
            (linkEquiv
              (DataLink
                (sws ++ (Switch swId pts tbl inp' outp ctrlm switchm):: sws0)
                src [pk] (swId,pt) hasSrc' hasDst')) :: 
            (MapEquivLoc links0))
           ofLinks ctrl)
         tblsOK' linkTopoOK').
  Proof.
  Admitted.

    exists (st' : State sws (links ++ (Data

tblsOK' linkT
      
      tblsOK 
             ctrl
    

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



  Lemma LinksHaveDst_untouched : forall 
    {swId tbl pts sws sws0 links ofLinks ctrl
    inp outp ctrlm switchm tbl' inp' outp' ctrlm' switchm' },
    LinksHaveDst
      (State (sws ++ (Switch swId pts tbl inp outp ctrlm switchm) :: sws0)
             links
             ofLinks ctrl) ->
    LinksHaveDst 
      (State (sws ++ (Switch swId pts tbl' inp' outp' ctrlm' switchm') :: sws0)
             links
             ofLinks ctrl).
  Admitted.   

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
    destruct devices0.
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


    assert (swId0 = sw) as J0.
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
    assert (exists x, inp0 === ({|(pt,pk)|} <+> x)). admit.
    destruct H1 as [inp HEqInp].

    remember (process_packet tbl0 pt pk) as ToCtrl eqn:Hprocess. 
    destruct ToCtrl as [outp' packetIns].
    symmetry in Hprocess.    

    remember (PktProcess sw pts0 inp outp0 ctrlm0 switchm0
                Hprocess
                sws sws0 links0 ofLinks0
                ctrl0) as Hstep.
    clear HeqHstep.
    match goal with
      | [ H : step _ _ ?S |- _  ] => 
        exists 
          (ConcreteState S
            (FlowTablesSafe_untouched concreteState_flowTableSafety0)
            (ConsistentDataLinks_untouched concreteState_consistentDataLinks0)
            (LinksHaveSrc_untouched linksHaveSrc0)
            (LinksHaveDst_untouched linksHaveDst0))
    end.
    split.
    unfold inverse_relation.
    unfold bisim_relation.
    unfold relate.
    simpl.
    rewrite -> map_app.
    autorewrite with bag using simpl.

    assert (FlowTableSafe sw tbl0) as Z.
      unfold FlowTablesSafe in concreteState_flowTableSafety0.
      apply concreteState_flowTableSafety0 with 
        (pts := pts0)
        (inp := inp0)
        (outp := outp0)
        (ctrlm := ctrlm0)
        (switchm := switchm0).
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
            (ConsistentDataLinks_untouched concreteState_consistentDataLinks0)
            (LinksHaveSrc_untouched linksHaveSrc0)
            (LinksHaveDst_untouched linksHaveDst0))
    end.
    apply StepEquivSwitch.
      unfold Equivalence.equiv.
      apply SwitchEquiv; try solve [ apply reflexivity | auto ].
    match goal with
      | [ H : step _ _ ?st |- _ ] =>
        apply multistep_obs with 
          (a0 := ConcreteState st
            (FlowTablesSafe_untouched concreteState_flowTableSafety0)
            (ConsistentDataLinks_untouched concreteState_consistentDataLinks0)
            (LinksHaveSrc_untouched linksHaveSrc0)
            (LinksHaveDst_untouched linksHaveDst0))
    end.
    exact Hstep.
    apply multistep_nil.

    (* Case where packet is in the output buffer. *)
    
    (* Proof sketch: We can assume that (topo sw pt) is defined, which implies
       that a data-link exists from this switch to some destination, dst.
       We reach the final state in two steps: SendDataLink followed by
       PktProcess.*)

    apply mem_unions_map in HMemOutp.
    destruct HMemOutp as [[pt0 pk0] [HIn HMemOutp]].
    simpl in HMemOutp.
    destruct (topo (swId0, pt0)).
    Focus 2. simpl in HMemOutp. inversion HMemOutp.
    destruct p.
    simpl in HMemOutp. inversion HMemOutp. clear HMemOutp.
    rewrite <- H2. clear s H2.

    apply mem_in_to_list in HIn.
    apply (mem_split _) in HIn.
    destruct HIn as [outp' HIn].
    rewrite <- H4 in *. clear pk0 H4.
    
    (* TODO(arjun): just how did i figure out (sw,pt) goes here *)
    assert (exists pks, In (DataLink (swId0,pt0) pks (sw,pt)) links0).
      admit.

    destruct H1 as [pks  Hlink].
    remember Hlink as Hlink_copy eqn:X.
    clear X.
    apply in_split in Hlink_copy.
    destruct Hlink_copy as [links [links1 Hlink_eq]].

    remember (SendDataLink swId0 pts0 tbl0 inp0 pt0 pk outp' ctrlm0
      switchm0 pks (sw,pt) sws sws0 links links0) as step1.

    (* Now we need to conclude that the switch with id ws exists! *)
    unfold LinksHaveDst in linksHaveDst0.
    simpl in linksHaveDst0.
    destruct (linksHaveDst0 sw pt (swId0,pt0) pks Hlink) as 
      [dst_sw [HDstIn [HDstIdEq HDstPtIn]]].
    destruct dst_sw.
    simpl in *.
    rewrite <- HDstIdEq in *.
    clear swId1 HDstIdEq.
Check RecvDataLink.

    remember (PktProcess sw

      (Se

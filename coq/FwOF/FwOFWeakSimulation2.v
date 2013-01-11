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
Require FwOF.FwOFRelationLemmas.

Local Open Scope list_scope.
Local Open Scope equiv_scope.
Local Open Scope bag_scope.

Module Make (Import Atoms : ATOMS).

  Module RelationLemmas := FwOF.FwOFRelationLemmas.Make (Atoms).
  Import RelationLemmas.
  Import RelationLemmas.Relation.
  Import RelationLemmas.Concrete.

     
  Lemma SimpleDraimWire : forall sws (swId : switchId) pts tbl inp outp 
    ctrlm switchm sws0  links src pks0 pks swId pt links0 ofLinks ctrl,
     exists inp',
     multistep step
      (State 
        (sws ++ (Switch swId pts tbl inp outp ctrlm switchm) :: sws0)
        (links ++ (DataLink src (pks0 ++ pks) (swId,pt)) :: links0)
        ofLinks ctrl)
      nil
      (State 
        (sws ++ (Switch swId pts tbl inp' outp ctrlm switchm) :: sws0)
        (links ++ (DataLink src pks0 (swId,pt)) :: links0)
        ofLinks ctrl).
   Proof with auto.
     intros.
     generalize dependent inp0. 
     generalize dependent pks1.
     induction pks1 using rev_ind.
     intros.
     simpl in *.
     exists inp0...
     rewrite -> app_nil_r...
     (* inductive case *)
     intros. 
     destruct (IHpks1 ( ({| (pt, x) |}) <+> inp0)) as [inp1 IHstep].
     exists (inp1). 
     eapply multistep_tau.
     rewrite -> (app_assoc pks0 pks1).
     apply RecvDataLink.
     apply IHstep.
   Qed.

   Hint Resolve LinksHaveSrc_inv LinksHaveDst_inv LinkTopoOK_inv
     FlowTablesSafe_untouched LinksHaveSrc_untouched LinksHaveDst_untouched.

  Lemma DrainWire2 : DrainWire_statement.
  Proof with eauto with datatypes.
    unfold DrainWire_statement.
    intros.
    generalize dependent inp0.
    induction pks0 using rev_ind.
    (* Base case: no other packets in front of pk, so nothing needs to
       be drained. *)
    intros.
    exists inp0.
    exists tblsOK.
    exists linkTopoOK.
    exists linksHaveSrc0.
    exists linksHaveDst0.
    apply multistep_nil.
    (* inductive case *)
    intros.


    (* Conditions for applying the inductive hypothesis. *)
    assert
      (ConsistentDataLinks 
        (links0 ++ (DataLink src0 (pk::pks0) (swId0,pt)) :: links1)) as
      stepN_linksTopoOK...
    assert
      (FlowTablesSafe
        (sws ++ (Switch swId0 pts0 tbl0 ({|(pt,x)|} <+> inp0) 
          outp0 ctrlm0 switchm0) :: sws0)) as stepN_tblsOK...
    assert
      (LinksHaveSrc
        (sws ++ (Switch swId0 pts0 tbl0 ({|(pt,x)|} <+> inp0) 
          outp0 ctrlm0 switchm0) :: sws0)
        (links0 ++ (DataLink src0 (pk::pks0) (swId0,pt)) :: links1))
      as stepN_linksHaveSrc...
    assert
      (LinksHaveDst
        (sws ++ (Switch swId0 pts0 tbl0 ({|(pt,x)|} <+> inp0) 
          outp0 ctrlm0 switchm0) :: sws0)
        (links0 ++ (DataLink src0 (pk::pks0) (swId0,pt)) :: links1))
      as stepN_linksHaveDst...    
    remember 
      (IHpks0 stepN_linksTopoOK _ stepN_tblsOK stepN_linksHaveSrc
        stepN_linksHaveDst) as J eqn:X.
    clear X IHpks0.
    destruct J as [inpN [tblsOKN [linkTopoOKN [linksHaveSrcN
      [linksHaveDstN HstepN]]]]].
    exists inpN. exists tblsOKN. exists linkTopoOKN. exists linksHaveSrcN.
    exists linksHaveDstN.

    assert
      (ConsistentDataLinks
        (links0 ++ (DataLink src0 ((pk :: pks0) ++ [x]) (swId0,pt)) :: links1))
      as thisLinkTopoOK...
    assert 
      (LinksHaveSrc 
        (sws ++ (Switch swId0 pts0 tbl0 inp0 outp0 ctrlm0 switchm0) :: sws0)
        (links0 ++ (DataLink src0 ((pk::pks0) ++ [x]) (swId0,pt)) :: links1))
      as thisLinksHaveSrc...
    assert 
      (LinksHaveDst
        (sws ++ (Switch swId0 pts0 tbl0 inp0 outp0 ctrlm0 switchm0) :: sws0)
        (links0 ++ (DataLink src0 ((pk::pks0) ++ [x]) (swId0,pt)) :: links1))
      as thisLinksHaveDst...
    apply multistep_tau
      with (a0 := ConcreteState (State _ _ ofLinks0 ctrl0) tblsOK 
                    thisLinkTopoOK thisLinksHaveSrc thisLinksHaveDst).
    unfold concreteStep.
    simpl.
    apply StepEquivState.
    unfold Equivalence.equiv.
    apply  StateEquiv.
    apply reflexivity.

    (* Now we can RecvLink *)
    assert
      (ConsistentDataLinks
        (links0 ++ (DataLink src0 (pk :: pks0) (swId0,pt)) :: links1))
      as step2_linkTopoOK...
    assert 
      (LinksHaveSrc 
        (sws ++ 
          (Switch swId0 pts0 tbl0 ({|(pt,x)|}<+>inp0) outp0 ctrlm0 switchm0) 
          :: sws0)
        (links0 ++ (DataLink src0 (pk::pks0) (swId0,pt)) :: links1))
      as step2_linksHaveSrc...
    assert 
      (LinksHaveDst
        (sws ++ 
          (Switch swId0 pts0 tbl0 ({|(pt,x)|}<+>inp0) outp0 ctrlm0 switchm0)  ::
          sws0)
        (links0 ++ (DataLink src0 (pk::pks0) (swId0,pt)) :: links1))
      as step2_linksHaveDst...
    apply multistep_tau
      with (a0 := ConcreteState (State _ _ ofLinks0 ctrl0) stepN_tblsOK 
        stepN_linksTopoOK stepN_linksHaveSrc stepN_linksHaveDst).
    unfold concreteStep.
    simpl.
    rewrite -> app_comm_cons.
    apply RecvDataLink.

    apply HstepN.
    Grab Existential Variables.
    exact nil.
    exact nil.
    exact nil.
    exact nil.
  Qed.

  Arguments DrainWire2 [sws sws0 swId pts tbl inp outp ctrlm switchm src pk
                        pks links links0] ofLinks ctrl [pt] tblsOK linkTopoOK
                       linksHaveSrc linksHaveDst.

  Theorem weak_sim_2 :
    weak_simulation abstractStep concreteStep (inverse_relation bisim_relation).
  Proof with simpl;eauto.
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
       that (sw,pt,pk) is in one of the concrete components. Mem defines
       how this may be. *)
    assert (Mem (sw,pt,pk) (({|(sw,pt,pk)|}) <+> lps)) as J...
    (* By J, (sw,pt,pk) is in the left-hand side of H. By Mem_equiv,
       (sw,pt,pk) is also on the right-hand side too. *)
    destruct (Bag.Mem_equiv (sw,pt,pk) H J) as 
      [HMemSwitch | [ HMemLink | [HMemOFLink | HMemCtrl ]]].
    (* The packet in on a switch. *)
    apply Bag.Mem_unions in HMemSwitch.
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
            concreteState_consistentDataLinks0
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

    apply Bag.unpop_unions with (b := ({|(sw,pt,pk)|})).
    rewrite -> Bag.union_comm.
    rewrite -> Bag.union_assoc.
    rewrite -> (Bag.union_comm _ lps).
    rewrite -> H.
    rewrite -> (Bag.FromList_map_iff _ _ HEqInp).
    rewrite -> Bag.map_union.
    rewrite -> Bag.to_list_singleton.
    simpl.
    rewrite -> Bag.from_list_singleton.
    repeat rewrite -> Bag.union_assoc.

    bag_perm 100.
    match goal with
      | [ H : step ?st _ _ |- _ ] =>
        apply multistep_tau with 
          (a0 := ConcreteState st
            (FlowTablesSafe_untouched concreteState_flowTableSafety0)
            concreteState_consistentDataLinks0
            (LinksHaveSrc_untouched linksHaveSrc0)
            (LinksHaveDst_untouched linksHaveDst0))
    end.
    apply StepEquivState.
    simpl.
    unfold Equivalence.equiv.
    apply StateEquiv.
    unfold Equivalence.equiv.

    Axiom SwitchesEquiv_app : forall sws11 sws21 sws12 sws22,
      SwitchesEquiv sws11 sws21 ->
      SwitchesEquiv sws12 sws22 ->
      SwitchesEquiv (sws11 ++ sws12) (sws21 ++ sws22).
    apply SwitchesEquiv_app.
    apply reflexivity.
    simpl.
    split.
    apply SwitchEquiv; try solve [ apply reflexivity | auto ].
    apply reflexivity.
    match goal with
      | [ H : step _ _ ?st |- _ ] =>
        apply multistep_obs with 
          (a0 := ConcreteState st
            (FlowTablesSafe_untouched concreteState_flowTableSafety0)
            concreteState_consistentDataLinks0
            (LinksHaveSrc_untouched linksHaveSrc0)
            (LinksHaveDst_untouched linksHaveDst0))
    end.
    exact Hstep.
    apply multistep_nil.

    (*
     * Case where packet is in the output buffer. 
     *)
    
    (* Proof sketch: We can assume that (topo sw pt) is defined, which implies
       that a data-link exists from this switch to some destination, dst.
       We reach the final state in two steps: SendDataLink followed by
       PktProcess.*)
 
    (* 1. Rewrite outp0 as a union of (pt,pk) and some other bag.  *)
    apply Bag.mem_unions_map in HMemOutp.
    destruct HMemOutp as [[pt0 pk0] [HIn HMemOutp]].
    simpl in HMemOutp.
    remember (topo (swId0, pt0)) as Htopo.
    destruct Htopo;
      [ idtac | simpl in HMemOutp; inversion HMemOutp ].
    destruct p.
    simpl in HMemOutp. inversion HMemOutp. subst.
    rename pk0 into pk.
    clear HMemOutp.
    apply Bag.mem_in_to_list in HIn.
    apply (Bag.mem_split _) in HIn.
    destruct HIn as [outp' HIn].
    Check HIn.

    (* TODO(arjun): The ConsistentDataLink invariants states that a link's
       source and destination is reflected in the topo function. We need
       another invariant stating that if the topo function is defined for
       a pair of locations, then there must exist a link between them. *)
    assert (exists pks, In (DataLink (swId0,pt0) pks (s,p)) links0) as X.
      admit.
    destruct X as [pks  Hlink].

    (* 2. Rewrite links0 as the union of the link in Hlink and the rest. *)
    apply in_split in Hlink.
    destruct Hlink as [links01 [links02 Hlink]].
    subst.

    eexists.
    apply and_comm.
    split.
    remember (State (sws ++ (Switch swId0 pts0 tbl0 inp0
                                   (({|(pt0, pk)|}) <+> outp') ctrlm0 switchm0) 
                         :: sws0)
                    (links01 ++ (DataLink (swId0, pt0) pks (s, p)) :: links02)
                    ofLinks0
                    ctrl0) as dev1.
    assert (FlowTablesSafe (switches dev1)) as HtblsOK1.
      subst...
    assert (LinksHaveSrc (switches dev1) (links dev1)) as HLinksHaveSrc1.
      subst...
    assert (LinksHaveDst (switches dev1) (links dev1)) as HLinksHaveDst1.
      subst...
    subst.
    apply multistep_tau with 
      (a0 := ConcreteState _ HtblsOK1 concreteState_consistentDataLinks0
                           HLinksHaveSrc1 HLinksHaveDst1).
    apply StepEquivState.
    simpl.
    unfold Equivalence.equiv.
    apply StateEquiv.
    apply SwitchesEquiv_app.
    apply reflexivity.
    simpl.
    split.
    apply SwitchEquiv; try solve [ apply reflexivity | auto ].
    apply reflexivity.
    
    clear linksHaveDst0 linksHaveSrc0 concreteState_flowTableSafety0.

    remember
      (State (sws ++ (Switch swId0 pts0 tbl0 inp0 outp' ctrlm0 switchm0) :: sws0)
             (links01 ++ (DataLink (swId0, pt0) (pk::pks) (s, p)) :: links02)
             ofLinks0
             ctrl0) as dev2.
    assert (FlowTablesSafe (switches dev2)) as HtblsOK2.
      subst...
    assert (LinksHaveSrc (switches dev2) (links dev2)) as HLinksHaveSrc2.
      subst...
    assert (LinksHaveDst (switches dev2) (links dev2)) as HLinksHaveDst2.
      subst...
    assert (ConsistentDataLinks (links dev2)) as HLinksOK2.
      subst...
    subst.
    apply multistep_tau with 
      (a0 := ConcreteState _ HtblsOK2 HLinksOK2 HLinksHaveSrc2 HLinksHaveDst2).
    unfold concreteStep.
    apply SendDataLink.


    (* Establish that the destination switch exists. *)
    assert 
      (LinkHasDst
         (sws ++ (Switch swId0 pts0 tbl0 inp0 outp' ctrlm0 switchm0) :: sws0)
         (DataLink (swId0,pt0) (pk::pks) (s,p))) as J0.
      apply HLinksHaveDst2. simpl. auto with datatypes.
    unfold LinkHasDst in J0.
    destruct J0 as [switch2 [HSw2In [HSw2IdEq HSw2PtsIn]]].
    destruct switch2.
    simpl in *.
    subst.
    apply in_split in HSw2In.
    destruct HSw2In as [sws' [sws0' HSw2In]].
    remember 
      (State
         (sws' ++ (Switch swId1 pts1 tbl1 inp1 outp1 ctrlm1 switchm1) :: sws0')
         (links01 ++ (DataLink (swId0,pt0) (pk::pks) (swId1,p)) :: links02)
         ofLinks0
         ctrl0) as dev3.
    assert (FlowTablesSafe (switches dev3)) as HtblsOK3.
      subst.
      rewrite <- HSw2In...
    assert (ConsistentDataLinks (links dev3)) as HLinksOK3.
      subst.
      rewrite <- HSw2In...
    assert (LinksHaveSrc (switches dev3) (links dev3)) as HLinksHaveSrc3.
      subst.
      rewrite <- HSw2In...
    assert (LinksHaveDst (switches dev3) (links dev3)) as HLinksHaveDst3.
      subst.
      rewrite <- HSw2In...


    apply multistep_tau with 
      (a0 := ConcreteState _ HtblsOK3 HLinksOK3
                           HLinksHaveSrc3 HLinksHaveDst3).
    apply StepEquivState.
    simpl.
    unfold  Equivalence.equiv.
    subst.
    apply StateEquiv.
    rewrite -> HSw2In.
    apply reflexivity.

    subst.
    remember (DrainWire2 ofLinks0 ctrl0 HtblsOK3 HLinksOK3 HLinksHaveSrc3 HLinksHaveDst3) as Hdrain.
    clear HeqHdrain.
    destruct Hdrain as [inp4 [tblsOK4 [linkTopoOK4 [linksSrc4 [linksDst4 Hdrain]]]]].
    match goal with
      | |- multistep _ _ ?X _ => remember X
    end.
    rewrite <- app_nil_l in Heql.
    subst.
    eapply multistep_app.
    apply Hdrain.

    (* </dance> *)

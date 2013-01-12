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

  Axiom SwitchesEquiv_app :
    forall sws11 sws21 sws12 sws22,
      SwitchesEquiv sws11 sws21 ->
      SwitchesEquiv sws12 sws22 ->
      SwitchesEquiv (sws11 ++ sws12) (sws21 ++ sws22).

  Lemma pkt_process : forall
    {sws sws0 : list switch}
    {links : list dataLink}
    {ofLinks : list openFlowLink}
    {ctrl : controller}
    {swId : switchId}
    {pts : list portId}
    {tbl : flowTable}
    {inp outp : bag (portId * packet)}
    (pt : portId)
    (pk : packet)
    {ctrlm : bag fromController}
    {switchm : bag fromSwitch}
    {lps : bag (switchId * portId * packet)}
    (rel : lps ===
           Bag.unions (map relate_switch sws) <+>
           FromList (map (affixSwitch swId) (Bag.to_list inp)) <+>
           Bag.unions (map (transfer swId) (Bag.to_list outp)) <+>
           Bag.unions (map (select_packet_out swId) (Bag.to_list ctrlm)) <+>
           Bag.unions (map (select_packet_in swId) (Bag.to_list switchm)) <+>
           Bag.unions (map relate_switch sws0) <+>
           Bag.unions (map relate_dataLink links) <+>
           Bag.unions (map relate_openFlowLink ofLinks) <+>
           relate_controller ctrl)
    (tblsOK : 
       FlowTablesSafe 
         (sws ++ (Switch swId pts tbl ({|(pt,pk)|}<+>inp) outp ctrlm switchm) ::
              sws0))
    (linksTopoOK : ConsistentDataLinks links)
    (haveSrc :
       LinksHaveSrc
         (sws ++ (Switch swId pts tbl ({|(pt,pk)|}<+>inp) outp ctrlm switchm) ::
              sws0)
         links)
    (haveDst :
       LinksHaveDst
         (sws ++ (Switch swId pts tbl ({|(pt,pk)|}<+>inp) outp ctrlm switchm) ::
              sws0)
         links),
    exists outp0 switchm0 tblsOK0 linksTopoOK0 haveSrc0 haveDst0,
      inverse_relation 
        bisim_relation
        (Bag.unions (map (transfer swId) (abst_func swId pt pk)) <+> lps)
        (ConcreteState
           (State
              (sws ++
                   (Switch swId pts tbl inp outp0 ctrlm switchm0) ::
                   sws0)
              links
              ofLinks
              ctrl)
           tblsOK0
           linksTopoOK0
           haveSrc0
           haveDst0) /\
      concreteStep
        (ConcreteState
           (State
              (sws ++
                 (Switch swId pts tbl ({|(pt,pk)|}<+>inp) outp ctrlm switchm) ::
                 sws0)
              links
              ofLinks
              ctrl)
           tblsOK
           linksTopoOK
           haveSrc
           haveDst)
        (Some (swId,pt,pk))
        (ConcreteState
           (State
              (sws ++
                 (Switch swId pts tbl inp outp0 ctrlm switchm0) ::
                 sws0)
              links
              ofLinks
              ctrl)
           tblsOK0
           linksTopoOK0
           haveSrc0
           haveDst0).
  Proof with eauto with datatypes.
    intros.
    remember (process_packet tbl0 pt pk) as ToCtrl eqn:Hprocess. 
    destruct ToCtrl as [outp1 pktIns].
    symmetry in Hprocess.    
    remember (PktProcess swId0 pts0 inp0 outp0 ctrlm0 switchm0
                         Hprocess
                         sws sws0 links0 ofLinks0
                         ctrl0) as Hstep eqn:X.
    clear X.
    exists (FromList outp1 <+> outp0).
    exists (FromList (map (PacketIn pt) pktIns) <+> switchm0).
    exists (FlowTablesSafe_untouched tblsOK).
    exists linksTopoOK.
    exists (LinksHaveSrc_untouched haveSrc).
    exists (LinksHaveDst_untouched haveDst).
    split.
    (* Case 1 *)
    assert (FlowTableSafe swId0 tbl0) as Z.
      unfold FlowTablesSafe in tblsOK.
      eapply tblsOK...
    unfold FlowTableSafe in Z.
    remember (Z pt pk outp1 pktIns Hprocess) as Y eqn:X.
    clear X Z.

    unfold inverse_relation.
    unfold bisim_relation.
    unfold relate.
    simpl.
    rewrite -> map_app.
    autorewrite with bag using simpl.
    rewrite <- Y.
    rewrite -> rel.
    autorewrite with bag using simpl.
    bag_perm 100.
    (* Case 2. *)
    subst.
    unfold concreteStep.
    simpl.
    trivial.
  Qed.   


  Lemma step_exists :
    forall obs3 s1 sw pt pk lps obs1 obs2 s2 s3,
      multistep concreteStep s1 obs1 s2 ->
      multistep concreteStep s2 obs2 s3 ->
      inverse_relation 
        bisim_relation
        (Bag.unions (map (transfer sw) (abst_func sw pt pk)) <+> lps) s3 ->
      obs1 ++ obs2 = obs3 ->
      exists t : concreteState,
       inverse_relation 
         bisim_relation
         (Bag.unions (map (transfer sw) (abst_func sw pt pk)) <+> lps) t /\
       multistep concreteStep s1 obs3 t.
    Proof with eauto.
      intros.
      exists s3.
      split...
      rewrite <- H2.
      eapply multistep_app...
    Qed.

  Theorem weak_sim_2 :
    weak_simulation abstractStep concreteStep (inverse_relation bisim_relation).
  Proof with simpl;eauto with datatypes.
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


    rewrite -> in_map_iff in HMemInp.
    destruct HMemInp as [[pt0 pk0] [Haffix HMemInp]].
    simpl in Haffix.
    inversion Haffix.
    subst.
    apply Bag.mem_in_to_list in HMemInp.
    clear Haffix.
    eapply Bag.mem_split in HMemInp.
    destruct HMemInp as [inp HEqInp].

    remember (process_packet tbl0 pt pk) as ToCtrl eqn:Hprocess. 
    destruct ToCtrl as (outp',inPkts).

    eapply simpl_weak_sim...
    apply multistep_tau with
    (a0 := (State (sws ++ (Switch sw pts0 tbl0 (({|(pt,pk)|}) <+> inp) outp0
                                 ctrlm0 switchm0) :: sws0)
                  links0
                  ofLinks0
                  ctrl0)).
    apply StepEquivState.
    unfold Equivalence.equiv.
    apply StateEquiv.
    unfold Equivalence.equiv.
    apply SwitchesEquiv_app.
    apply reflexivity.
    simpl.
    split.
    apply SwitchEquiv; try solve [ eauto | apply reflexivity ].
    apply reflexivity.
    eapply multistep_obs.
    apply PktProcess. 
    instantiate (1 := inPkts).
    instantiate (1 := outp').
    symmetry. exact Hprocess.
    eapply multistep_nil.
    rewrite -> H.
    unfold relate.
    simpl.
    rewrite -> map_app.
    simpl.
    rewrite -> Bag.unions_app.
    simpl.
    repeat rewrite -> Bag.union_assoc.
    apply reflexivity.

    apply Swit
    simpl.
    simpl.
    Focus 3.
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
    apply Bag.mem_in_to_list in HMemInp.
    clear Haffix.
    eapply Bag.mem_split in HMemInp.
    destruct HMemInp as [inp HEqInp].
    assert (FromList (map (affixSwitch sw) (Bag.to_list inp0)) ===
            FromList (map (affixSwitch sw) (Bag.to_list ({|(pt,pk)|}<+>inp))))
           as X.
      eapply Bag.FromList_map_iff...
    rewrite -> X in H.
    clear X.
    rewrite -> Bag.map_union in H.
    autorewrite with bag in H using (simpl in H).

    rewrite <- Bag.union_assoc in H.
    rewrite -> (Bag.union_comm _ _ ({|(sw,pt,pk)|})) in H.
    rewrite -> Bag.union_assoc in H.
    apply Bag.unpop_unions in H.
    simpl in *.
    remember
      (pkt_process 
         pt pk H 
         (FlowTablesSafe_untouched concreteState_flowTableSafety0)
         concreteState_consistentDataLinks0
         (LinksHaveSrc_untouched linksHaveSrc0)
         (LinksHaveDst_untouched linksHaveDst0)) as X.
    clear HeqX.
    destruct X as [outp1 [switchm1 [tblsOK1 [linksTopoOK1 [haveSrc1 [haveDst1 
                  [Hrel Hstep]]]]]]].
    match goal with
      | [ _ : concreteStep ?s1 _ ?s2 |- _ ] => 
        remember s1 as S1; remember s2 as S2
    end.
    exists S2.
    split...
    apply multistep_tau with
    (a0 := S1).
    apply StepEquivState.
    subst.
    simpl.
    unfold Equivalence.equiv.
    apply StateEquiv.
    unfold Equivalence.equiv.
    apply SwitchesEquiv_app.
    apply reflexivity.
    simpl.
    split.
    apply SwitchEquiv; try solve [ auto | apply reflexivity ].
    apply reflexivity.
    apply multistep_obs with (a0 := S2)...

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


    (* 3. Establish that the destination switch exists. *)
    assert 
      (LinkHasDst
         (sws ++ (Switch swId0 pts0 tbl0 inp0 outp0 ctrlm0 switchm0) :: sws0)
         (DataLink (swId0,pt0) pks (s,p))) as J0.
      apply linksHaveDst0...
    unfold LinkHasDst in J0.
    destruct J0 as [switch2 [HSw2In [HSw2IdEq HSw2PtsIn]]].
    destruct switch2.
    simpl in *.
    subst.
    apply in_split in HSw2In.
    destruct HSw2In as [sws' [sws0' HSw2In]].
    subst.
    remember (process_packet tbl1 p pk) as X eqn:Hprocess.
    destruct X as [outp1' pktIns].


    (* First step rewrites outp0 to outp0' <+> (pt0,pk) *)
    remember
      (State (sws ++ (Switch swId0 pts0 tbl0 inp0
                             (({|(pt0, pk)|}) <+> outp') ctrlm0 switchm0) 
                  :: sws0)
             (links01 ++ (DataLink (swId0, pt0) pks (swId1, p)) :: links02)
             ofLinks0
             ctrl0) as dev1.
    assert (FlowTablesSafe (switches dev1)) as HtblsOK1.
      subst...
    assert (LinksHaveSrc (switches dev1) (links dev1)) as HLinksHaveSrc1.
      subst...
    assert (LinksHaveDst (switches dev1) (links dev1)) as HLinksHaveDst1.
      subst...
    subst.
    remember 
      (ConcreteState _ HtblsOK1 concreteState_consistentDataLinks0
                     HLinksHaveSrc1 HLinksHaveDst1) as state1.
    match goal with
      | |- exists _, _ /\ multistep _ ?s _ _ => remember s as state0
    end.
    assert (concreteStep state0 None state1) as step0.
      apply StepEquivState.
      simpl.
      unfold Equivalence.equiv.
      subst.
      apply StateEquiv.
      apply SwitchesEquiv_app.
      apply reflexivity.
      simpl.
      split.
      apply SwitchEquiv; try solve [ apply reflexivity | auto ].
      apply reflexivity.

    (* Step 2 is to emit the packet on the wire. *)
    remember
      (State (sws ++ (Switch swId0 pts0 tbl0 inp0 outp' ctrlm0 switchm0) :: sws0)
             (links01 ++ (DataLink (swId0, pt0) (pk::pks) (swId1, p)) :: links02)
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
    remember
      (ConcreteState _ HtblsOK2 HLinksOK2 HLinksHaveSrc2 HLinksHaveDst2) 
      as state2.
    assert (concreteStep state1 None state2) as step1.
      unfold concreteStep.
      subst.
      apply SendDataLink.

   (* Step 3. drain the other packets (pk) in front on the wire. *)
    remember
      (sws' ++ (Switch swId1 pts1 tbl1 inp1 outp1 ctrlm1 switchm1) :: sws0') as
        switches2.
    assert (FlowTablesSafe switches2) as tblsOK3.
      rewrite <- HSw2In...
    assert (LinksHaveSrc switches2
             (links01 ++ (DataLink (swId0,pt0) (pk::pks) (swId1,p)) :: links02))
             as linksSrc3.
      rewrite <- HSw2In...
    assert (LinksHaveDst switches2
             (links01 ++ (DataLink (swId0,pt0) (pk::pks) (swId1,p)) :: links02))
             as linksDst3.
      rewrite <- HSw2In...
    
    subst.
    simpl in *.
    remember (DrainWire2 ofLinks0 ctrl0 tblsOK3 HLinksOK2 linksSrc3 linksDst3)
             as step2 eqn:X; clear X.
    destruct step2 as
        [inp4 [tblsOK4 [linkTopoOK4 [linksSrc4 [linksDst4 step2]]]]].
    match goal with
      | [ step2 : multistep _ ?s2 _ ?s3 |- _ ] =>
        remember s2 as state2; remember s3 as state3
    end.

    (* Step 4. prime to pull off the wire. *)
    remember 
      (State
         (sws' ++ (Switch swId1 pts1 tbl1 inp4 outp1 ctrlm1 switchm1) :: sws0')
         (links01 ++ (DataLink (swId0,pt0) (nil ++ [pk])(swId1,p)) :: links02)
         ofLinks0
         ctrl0) as dev4.
    assert (FlowTablesSafe (switches dev4)) as tblsOK5. subst...
    assert (ConsistentDataLinks (links dev4)) as linksTopoOK5. subst...
    assert (LinksHaveSrc (switches dev4) (links dev4)) as linksSrc5. subst...
    assert (LinksHaveDst (switches dev4) (links dev4)) as linksDst5. subst...
    assert
      (concreteStep state3 None
                    (ConcreteState _ tblsOK5 linksTopoOK5 linksSrc5 linksDst5))
      as step3.
      apply StepEquivState.
      simpl.
      unfold Equivalence.equiv.
      subst.
      apply StateEquiv.
      apply reflexivity.
    match goal with
      | [ step2 : concreteStep state3 None ?s4 |- _ ] =>  remember s4 as state4
    end.
    
    
    (* Step 5. Actually pull off the wire. *)
    remember 
      (State
         (sws' ++ (Switch swId1 pts1 tbl1 ({|(p,pk)|} <+> inp4) outp1 ctrlm1 
                          switchm1) :: sws0')
         (links01 ++ (DataLink (swId0,pt0) nil (swId1,p)) :: links02)
         ofLinks0
         ctrl0) as dev5.
    assert (FlowTablesSafe (switches dev5)) as tblsOK6. subst...
    assert (ConsistentDataLinks (links dev5)) as linksOK6. subst...
    assert (LinksHaveSrc (switches dev5) (links dev5)) as haveSrc6. subst...
    assert (LinksHaveDst (switches dev5) (links dev5)) as haveDst6. subst...
    remember (ConcreteState _ tblsOK6 linksOK6 haveSrc6 haveDst6) as state5.
    assert (concreteStep state4 None state5) as step4.
      subst.
      apply RecvDataLink.
    
    (* Step 6. do pktprocess. *)
    assert 
      (lps ===
           Bag.unions (map relate_switch sws') <+>
           FromList (map (affixSwitch swId1) (Bag.to_list inp4)) <+>
           Bag.unions (map (transfer swId1) (Bag.to_list outp1)) <+>
           Bag.unions (map (select_packet_out swId1) (Bag.to_list ctrlm1)) <+>
           Bag.unions (map (select_packet_in swId1) (Bag.to_list switchm1)) <+>
           Bag.unions (map relate_switch sws0') <+>
           Bag.unions (map relate_dataLink 
                         (links01 ++ (DataLink (swId0,pt0) nil (swId1, p)) ::
                                  links02)) <+>
           Bag.unions (map relate_openFlowLink ofLinks0) <+>
           relate_controller ctrl0) as cheat. admit.
    
    simpl in *. subst.
    remember (pkt_process p pk cheat tblsOK6 linksOK6 haveSrc6 haveDst6) 
      as W eqn:X; clear X.
    destruct W as [outp6 [switchm6 [tblsOK7 [linksTopoOK7 [haveSrc7 [haveDst7 
                  [Hrel step5]]]]]]].
    match goal with
      | [ _ : concreteStep ?s1 _ ?s2 |- _ ] => 
        remember s1 as state5; remember s2 as state6
    end.

    exists state6.
    apply and_comm.
    split.
    Check state1.
    apply multistep_app with (s2 := state6
    apply multistep_tau.
    .

    apply multistep_obs with (a0 := S2)...
    subst.
    eapply multistep_nil.
    assert (multistep concreteStep S2 nil S2).






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
    clear inp1 HSw2In HtblsOK3 HLinksOK3 HLinksHaveSrc3 HLinksHaveDst3 Hdrain.

    assert ([pk] = nil ++ [pk]) as X. simpl...
    remember 
      (State
         (sws' ++ (Switch swId1 pts1 tbl1 inp4 outp1 ctrlm1 switchm1) :: sws0')
         (links01 ++ (DataLink (swId0,pt0) (nil ++ [pk])(swId1,p)) :: links02)
         ofLinks0
         ctrl0) as dev4.
    assert (FlowTablesSafe (switches dev4)) as HtblsOK4. subst...
    assert (ConsistentDataLinks (links dev4)) as HLinksOK4. subst...
    assert (LinksHaveSrc (switches dev4) (links dev4)) as HLinksSrc4. subst...
    assert (LinksHaveDst (switches dev4) (links dev4)) as HLinksDst4. subst...
    apply multistep_tau with 
      (a0 := ConcreteState _ HtblsOK4 HLinksOK4 HLinksSrc4 HLinksDst4).
    apply StepEquivState.
    simpl.
    unfold  Equivalence.equiv.
    subst.
    apply StateEquiv.
    apply reflexivity.

    remember 
      (State
         (sws' ++ (Switch swId1 pts1 tbl1 ({|(p,pk)|} <+> inp4) outp1 ctrlm1 
                          switchm1) :: sws0')
         (links01 ++ (DataLink (swId0,pt0) nil (swId1,p)) :: links02)
         ofLinks0
         ctrl0) as dev5.
    assert (FlowTablesSafe (switches dev5)) as HtblsOK5.
      subst...
    assert (ConsistentDataLinks (links dev5)) as HLinksOK5.
      subst...
    assert (LinksHaveSrc (switches dev5) (links dev5)) as HLinksHaveSrc5.
      subst...
    assert (LinksHaveDst (switches dev5) (links dev5)) as HLinksHaveDst5.
      subst...
    apply multistep_tau with 
      (a0 := ConcreteState _ HtblsOK5 HLinksOK5
                           HLinksHaveSrc5 HLinksHaveDst5).
    subst.
    unfold concreteStep.
    apply RecvDataLink.

    clear dev4 Heqdev4 HtblsOK4 HLinksOK4 HLinksSrc4 HLinksDst4.
    
    subst.
    assert 
      (lps ===
           Bag.unions (map relate_switch sws') <+>
           FromList (map (affixSwitch swId1) (Bag.to_list inp4)) <+>
           Bag.unions (map (transfer swId1) (Bag.to_list outp1)) <+>
           Bag.unions (map (select_packet_out swId1) (Bag.to_list ctrlm1)) <+>
           Bag.unions (map (select_packet_in swId1) (Bag.to_list switchm1)) <+>
           Bag.unions (map relate_switch sws0') <+>
           Bag.unions (map relate_dataLink 
                         (links01 ++ (DataLink (swId0,pt0) nil (swId1, p)) ::
                                  links02)) <+>
           Bag.unions (map relate_openFlowLink ofLinks0) <+>
           relate_controller ctrl0) as cheat. admit.
    
clear X.
    remember (pkt_process p pk cheat HtblsOK5 HLinksOK5 HLinksHaveSrc5 
                       HLinksHaveDst5) as W eqn:X.
    clear X.

    destruct W as [outp6 [switchm6 [tblsOK6 [linksTopoOK6 [haveSrc6 [haveDst6 
                  [Hrel Hstep]]]]]]].
    match goal with
      | [ _ : concreteStep ?s1 _ ?s2 |- _ ] => 
        remember s1 as S1; remember s2 as S2
    end.
    apply multistep_obs with (a0 := S2)...
    subst.
    eapply multistep_nil.
    assert (multistep concreteStep S2 nil S2).
      apply multistep_nil.
      Show Existentials.
instantiate (2 := S1).
      subst.
    instantiate.
    instantiate 
      (1 := 
    Check (ConcreteState
           (State (sws' ++ (Switch swId1 pts1 tbl1 (({|(p, pk)|}) <+> inp4)
                                   outp1 ctrlm1 switchm1) :: sws0')
                  (links01 ++ (DataLink (swId0, pt0) nil (swId1,p)) :: links02)
                  ofLinks0
                  ctrl0)
           HtblsOK5 HLinksOK5 HLinksHaveSrc5 HLinksHaveDst5).

    eapply multistep_nil.


    assert ([(swId1,p,pk)] = nil ++ [(swId1,p,pk)]) as X...
    rewrite -> X.
    rewrite <- (@app_nil_l (switchId*portId*packet)).
    apply multistep_app.

    

 : ({|(swId1, p, pk)|}) <+> lps ===
      Bag.unions (map relate_switch sws) <+>
      FromList (map (affixSwitch swId0) (Bag.to_list inp0)) <+>
      Bag.unions (map (transfer swId0) (Bag.to_list outp0)) <+>
      Bag.unions (map (select_packet_out swId0) (Bag.to_list ctrlm0)) <+>
      Bag.unions (map (select_packet_in swId0) (Bag.to_list switchm0)) <+>
      Bag.unions (map relate_switch sws0) <+>
      Bag.unions
        (map relate_dataLink
           (links01 ++
            {| src := (swId0, pt0); pks := pks; dst := (swId1, p) |}
            :: links02)) <+>
      Bag.unions (map relate_openFlowLink ofLinks0) <+>
      relate_controller ctrl0
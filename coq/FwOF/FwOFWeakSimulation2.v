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

  Axiom SwitchesEquiv_app :
    forall sws11 sws21 sws12 sws22,
      SwitchesEquiv sws11 sws21 ->
      SwitchesEquiv sws12 sws22 ->
      SwitchesEquiv (sws11 ++ sws12) (sws21 ++ sws22).

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

    (* ********************************************************************** *)
    (* Case 1 : Packet is in an input buffer                                  *)
    (* ********************************************************************** *)
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

    (* ********************************************************************** *)
    (* Case 2 : Packet is in an output buffer                                 *)
    (* ********************************************************************** *)

    (* Proof sketch: We can assume that (topo sw pt) is defined, which implies
       that a data-link exists from this switch to some destination, dst.
       We reach the final state in two steps: SendDataLink followed by
       PktProcess.*)
 
    (* 1. Rewrite outp0 as a union of (pt,pk) and some other bag.  *)
    apply Bag.mem_unions_map in HMemOutp.
    destruct HMemOutp as [[pt0 pk0] [HIn HMemOutp]].
    simpl in HMemOutp.
    remember (topo (swId0, pt0)) as Htopo.
    destruct Htopo.
    2: solve [ simpl in HMemOutp; inversion HMemOutp ].
    destruct p.
    simpl in HMemOutp. inversion HMemOutp. subst.
    rename pk0 into pk.
    clear HMemOutp.
    apply Bag.mem_in_to_list in HIn.
    apply (Bag.mem_split _) in HIn.
    destruct HIn as [outp' HIn].

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

    eapply simpl_weak_sim...
    (* First step rewrites outp0 to outp' <+> (pt0,pk) *)
    apply multistep_tau with
    (a0 := State (sws ++ (Switch swId0 pts0 tbl0 inp0 (({|(pt0,pk)|}) <+> outp')
                                 ctrlm0 switchm0) :: sws0)
                 (links01 ++ (DataLink (swId0,pt0) pks (swId1,p)) :: links02)
                 ofLinks0
                 ctrl0).
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

    apply multistep_tau with
    (a0 := State (sws ++ (Switch swId0 pts0 tbl0 inp0 outp'
                                 ctrlm0 switchm0) :: sws0)
                 (links01 ++ (DataLink (swId0,pt0) (pk::pks) (swId1,p)) :: links02)
                 ofLinks0
                 ctrl0).
    apply SendDataLink.

    assert 
      (LinkHasDst
         (sws ++ (Switch swId0 pts0 tbl0 inp0 outp' ctrlm0 switchm0) :: sws0)
         (DataLink (swId0,pt0) (pk::pks) (swId1,p))) as J0.
      remember  (LinksHaveDst_untouched (switchm' := switchm0) (ctrlm' := ctrlm0) 
        (outp' := outp')(inp' := inp0) (tbl' := tbl0)
        (LinksHaveDst_inv pks (pk::pks) linksHaveDst0)) as X.
      unfold LinksHaveDst in X.
      apply X...
    unfold LinkHasDst in J0.
    destruct J0 as [switch3 [HSw3In [HSw3IdEq HSw3PtsIn]]].
    destruct switch3.
    simpl in *.
    subst.
    apply in_split in HSw3In.
    destruct HSw3In as [sws'' [sws0'' HSw3In]].
    subst.
    rewrite -> HSw3In.
    Check (SimpleDraimWire sws'' swId2 pts2 tbl2 inp2 outp2 ctrlm2 switchm2 sws0''
                           links01 (swId0,pt0) [pk] pks swId2 p links02 ofLinks0 ctrl0).

    destruct (SimpleDraimWire sws'' swId2 pts2 tbl2 inp2 outp2 ctrlm2 switchm2 sws0''
      links01 (swId0,pt0) [pk] pks swId2 p links02 ofLinks0 ctrl0) as
    [inp4 step].
    eapply multistep_app with (obs2 := [(swId2,p,pk)]).
      exact step.

    assert ([pk] = nil ++ [pk]) as X...
    rewrite -> X.
    clear X.
    eapply multistep_tau.
    apply RecvDataLink.

    eapply multistep_obs.
    apply PktProcess.
    instantiate (1 := pktIns).
    instantiate (1 := outp1').
    symmetry.
    assert (tbl1 = tbl2) as X. admit. (* TODO(arjun): need unique ids *)
    subst.
    trivial.
    (* TODO(arjun): crap about instantiating in right environment GG *)
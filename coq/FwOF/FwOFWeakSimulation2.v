Set Implicit Arguments.

Require Import Coq.Lists.List.
Require Import Coq.Classes.Equivalence.
Require Import Coq.Classes.EquivDec.
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
    ctrlm switchm links src pks0 pks swId pt links0 ofLinks ctrl,
     exists inp',
     multistep step
      (State 
        ({|Switch swId pts tbl inp outp ctrlm switchm|} <+> sws)
        (links ++ (DataLink src (pks0 ++ pks) (swId,pt)) :: links0)
        ofLinks ctrl)
      nil
      (State 
        ({|Switch swId pts tbl inp' outp ctrlm switchm|} <+> sws)
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

  Lemma ObserveFromOutp : forall pktOuts pktIns pk pt0 pt1
    swId0 pts0 tbl0 inp0 outp0 ctrlm0 switchm0
    swId1 pts1 tbl1 inp1 outp1 ctrlm1 switchm1
    sws pks links0 links1 ofLinks0 ctrl0,
    (pktOuts, pktIns) = process_packet tbl1 pt1 pk ->
    Some (swId1,pt1) = topo (swId0,pt0) ->
    multistep step
      (State 
        (({|Switch swId0 pts0 tbl0 inp0 ({|(pt0,pk)|} <+> outp0) 
                  ctrlm0 switchm0|}) <+>
         ({|Switch swId1 pts1 tbl1 inp1 outp1 ctrlm1 switchm1|}) <+>
         sws)
        (links0 ++ (DataLink (swId0,pt0) pks (swId1,pt1)) :: links1)
        ofLinks0 ctrl0)
      [(swId1,pt1,pk)]
      (State 
        (({|Switch swId0 pts0 tbl0 inp0 outp0 ctrlm0 switchm0|}) <+>
         ({|Switch swId1 pts1 tbl1
                  (FromList (map (fun pk => (pt1,pk)) pks) <+> inp1) 
                  (FromList pktOuts <+> outp1)
                  ctrlm1
                  (FromList (map (PacketIn pt1) pktIns) <+> switchm1)|}) <+>
         sws)
        (links0 ++ (DataLink (swId0,pt0) nil (swId1,pt1)) :: links1)
        ofLinks0 ctrl0).
  Proof with simpl;eauto with datatypes.
    intros.

    eapply multistep_tau.
    apply SendDataLink.
    apply multistep_tau with
    (a0 := State
             (({|Switch swId1 pts1 tbl1 inp1 outp1 ctrlm1 switchm1|}) <+>
              ({|Switch swId0 pts0 tbl0 inp0 outp0 ctrlm0 switchm0|}) <+>
              sws)
             (links0 ++ (DataLink (swId0,pt0) ([pk]++pks0) (swId1,pt1))::links1)
             ofLinks0
             ctrl0).
      apply StepEquivState.
      apply StateEquiv.
      rewrite <- Bag.union_assoc.
      rewrite -> (Bag.union_comm _ ({|Switch swId0 pts0 tbl0 inp0 outp0 
                                             ctrlm0 switchm0|})).
      rewrite -> Bag.union_assoc.
      apply Bag.pop_union; apply reflexivity.
      

    destruct (SimpleDraimWire 
      (({|Switch swId0 pts0 tbl0 inp0 outp0 ctrlm0 switchm0|}) <+> sws)
      swId1 pts1 tbl1 inp1 outp1 ctrlm1 switchm1 
      links0 (swId0,pt0) [pk] pks0 swId1 pt1 links1 ofLinks0 ctrl0)
    as [inp4 step].
    eapply multistep_app with (obs2 := [(swId1,pt1,pk)]).
    exact step.
    
    assert ([pk] = nil ++ [pk]) as X... rewrite -> X. clear X.

    eapply multistep_tau.
    apply RecvDataLink.

    eapply multistep_obs.
    apply PktProcess.
    instantiate (1 := pktIns).
    instantiate (1 := pktOuts).
    symmetry...

    idtac "TODO(arjun): SimpleDrainWire needs to describe its output.".
    admit.
    simpl...
  Qed.

  (** This lemma relies on the following property: we can pick a
      packet located anywhere (e.g., a packet at the controller or in
      a PacketIn/PacketOut message) and construct a trace that
      observes *only* that packet. This is not obvious, because
      barriers may force other messages to be processed first.
      However, barriers cannot force a switch to emit a packet.

      Consider [PacketOut pt pk] sent to switch [sw1], followed by a 
      [BarrierRequest n]. Even if the controller waits for [BarrierReply n],
      that does not force pk to be transfered to its destination.

      Now, consider a controller that waits for a [PacketIn pk] at the
      destination, thereby forcing a different observation. We have to
      rule out such controllers with our liveness property. *)
(*
  Lemma ObserveFromController : forall dstSw dstPt pk src
    In (PacketOut srcPt pk) ctrlm0 ->
    Some (dstSw,dstPt) = topo srcSw dstPt ->
    relate (State sws0 links0 ofLinks0 ctrl0) === ({|dstSw,dstPt,pk|}) <+> lps ->
    exists sws1 links1,
      multistep (State sws0 links0 ofLinks0 ctrl0)
                [(dstSw,dstPt,pk)]
                (State sws1 links1 ofLinks1 ctrl0).
    Bag.Mem (Switch 
*)
  
   
  Hint Resolve switch_equiv_is_Equivalence.
  Hint Constructors eq.
  Existing Instances switch_Equivalence switch_eqdec packet_eqdec portId_eqdec.

  Instance Equivalence_eq `(A : Type) : Equivalence (@eq A).
  Proof with auto.
    split.
    unfold Reflexive...
    unfold Symmetric...
    unfold Transitive. intros. subst...
  Qed.
 
  Instance ptPkt_eqdec : EqDec (portId * packet) eq := 
    prod_eqdec portId_eqdec packet_eqdec.

  Instance swPtPk_eqdec : EqDec (switchId * portId * packet) eq :=
    prod_eqdec (prod_eqdec switchId_eqdec portId_eqdec) packet_eqdec.

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
    assert (Mem (sw,pt,pk) (({|(sw,pt,pk)|}) <+> lps)) as J.
      apply Bag.mem_union. left. simpl. apply reflexivity.
    (* By J, (sw,pt,pk) is in the left-hand side of H. By Mem_equiv,
       (sw,pt,pk) is also on the right-hand side too. *)
    destruct (Bag.Mem_equiv (sw,pt,pk) H J) as 
      [HMemSwitch | [ HMemLink | [HMemOFLink | HMemCtrl ]]].
    (* The packet in on a switch. *)
    apply Bag.Mem_unions in HMemSwitch.
    destruct HMemSwitch as [switch_abst [ Xin Xmem ]].
    simpl in Xin.
    simpl in H.
    apply Bag.in_map_mem  with 
      (E := switch_equiv_is_Equivalence) in Xin.
    destruct Xin as [switch [Xrel Xin]].
    subst.
    apply Bag.mem_split with (ED := switch_eqdec) in Xrel.
    destruct Xrel as [sws Xrel].
    destruct switch.
    simpl in Xmem.
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
    apply Bag.mem_in_to_list with (R := eq) (E := Equivalence_eq) in HMemInp.
    clear Haffix.
    eapply Bag.mem_split with (ED := ptPkt_eqdec) in HMemInp.
    destruct HMemInp as [inp HEqInp].

    remember (process_packet tbl0 pt pk) as ToCtrl eqn:Hprocess. 
    destruct ToCtrl as (outp',inPkts).

    eapply simpl_weak_sim...
    apply multistep_tau with
    (a0 := (State ({|Switch sw pts0 tbl0 (({|(pt,pk)|}) <+> inp) outp0
                                 ctrlm0 switchm0|} <+> sws)
                  links0
                  ofLinks0
                  ctrl0)).
    apply StepEquivState.
    apply StateEquiv.
    rewrite -> Xrel.
    do 2 rewrite -> (Bag.union_comm _ _ sws).
    apply Bag.pop_union_l.
    apply Bag.equiv_singleton.
    apply SwitchEquiv; try solve [ eauto | apply reflexivity ].
    eapply multistep_obs.
    apply PktProcess. 
    instantiate (1 := inPkts).
    instantiate (1 := outp').
    symmetry. exact Hprocess.
    eapply multistep_nil.
    rewrite -> H.
    unfold relate.
    simpl.
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
    eapply Bag.mem_split with (ED := swPtPk_eqdec) in HMemOutp.
    destruct HMemOutp as [HEmpty HSingleton].
    simpl in HSingleton.
    remember (topo (swId0,pt0)) as Htopo.
    destruct Htopo.
    destruct p.
    apply Bag.singleton_equiv in HSingleton.
    destruct HSingleton as [HSingleton HEmpty'].
    clear HEmpty HEmpty'.
    inversion HSingleton. subst. clear HSingleton.
    apply Bag.mem_in_to_list 
      with 
        (R := eq) (E := Equivalence_eq) in HIn.
    apply Bag.mem_split with (ED := ptPkt_eqdec) in HIn.
    destruct HIn as [outp' HIn].

    (* TODO(arjun): The ConsistentDataLink invariants states that a link's
       source and destination is reflected in the topo function. We need
       another invariant stating that if the topo function is defined for
       a pair of locations, then there must exist a link between them. *)
    assert (exists pks, In (DataLink (swId0,pt0) pks (sw,pt)) links0) as X.
      admit.
    destruct X as [pks Hlink].

    (* 2. Rewrite links0 as the union of the link in Hlink and the rest. *)
    apply in_split in Hlink.
    destruct Hlink as [links01 [links02 Hlink]].
    subst.

    (* 3. Establish that the destination switch exists. *)
    assert 
      (LinkHasDst
         switches0
         (DataLink (swId0,pt0) pks (sw,pt))) as J0.
      apply linksHaveDst0...
    unfold LinkHasDst in J0.
    destruct J0 as [switch2 [HSw2In [HSw2IdEq HSw2PtsIn]]].
    destruct switch2.
    simpl in *.
    subst.
    remember (process_packet tbl1 pt pk) as X eqn:Hprocess.
    destruct X as [outp1' pktIns].

    eapply Bag.mem_equiv with (ED := switch_eqdec) in HSw2In.
    2: exact Xrel.
    destruct HSw2In as [switch1 [HMem2 Hswitch2]].
    apply Bag.mem_union in HMem2.
    destruct HMem2. 

    idtac "TODO(arjun): src and dst switches are the same".
    admit.

    apply Bag.mem_split with (ED := switch_eqdec) in H1.
    destruct H1 as [sws0 H1].
    rewrite -> H1 in Xrel.

    apply simpl_weak_sim with
      (devs2 := 
        State ({| Switch swId0 pts0 tbl0 inp0 outp' ctrlm0 switchm0 |} <+>
               ({| Switch swId1 pts1 tbl1 
                          ((FromList (map (fun pk => (pt,pk)) pks)) <+> inp1) 
                          (FromList outp1' <+> outp1)
                        ctrlm1 (FromList (map (PacketIn pt) pktIns) <+> switchm1)|}) <+>
               sws0)
              (links01 ++ (DataLink (swId0,pt0) nil (swId1,pt)) :: links02)
              ofLinks0
              ctrl0).
    (* First step rewrites outp0 to outp' <+> (pt0,pk) *)
    apply multistep_tau with
    (a0 := State (({|Switch swId0 pts0 tbl0 inp0 (({|(pt0,pk)|}) <+> outp')
                            ctrlm0 switchm0|}) <+>
                  ({|Switch swId1 pts1 tbl1 inp1 outp1 ctrlm1 switchm1|}) <+>
                  sws0)
                 (links01 ++ (DataLink (swId0,pt0) pks (swId1,pt)) :: links02)
                 ofLinks0
                 ctrl0).
      apply StepEquivState.
      apply StateEquiv.
      rewrite -> Xrel.
      apply Bag.pop_union.
      apply Bag.equiv_singleton.
      apply SwitchEquiv; try solve [ apply reflexivity ].
      exact HIn.
      apply Bag.pop_union.
      apply Bag.equiv_singleton.
      apply symmetry...
      apply reflexivity.

    eapply ObserveFromOutp... (* #winning *)
    rewrite -> H.
    unfold relate.
    simpl.
    apply reflexivity.
    trivial.
    (* idiotic contradiction in HSingleton. Below we match the stupid goal directly
       so that we don't accidentally admit something else if the goal is discharged
       earlier due to refactoring. *)
    match goal with
      | [ H : ({||}) === ({|(sw, pt, pk)|}) <+> HEmpty |- _] => admit
    end.

    (* ********************************************************************** *)
    (* Case 3 : Packet is in a PacketOut message                              *)
    (* ********************************************************************** *)

    apply Bag.mem_unions_map in HMemCtrlm.
    destruct HMemCtrlm as [msg [HIn HMemCtrlm]].
    destruct msg.
    2: solve [ simpl in HMemCtrlm; inversion HMemCtrlm ]. (* not a barrier *)
    2: solve [ simpl in HMemCtrlm; inversion HMemCtrlm ]. (* not a flowmod *)
    simpl in HMemCtrlm.
    remember (topo (swId0,p)) as Htopo.
    destruct Htopo.
    2: solve [ simpl in HMemCtrlm; inversion HMemCtrlm ]. (* packet does not go poof *)
    destruct p1.
    simpl in HMemCtrlm.
    unfold Equivalence.equiv in HMemCtrlm.
    symmetry in HMemCtrlm. (* Prefer the names on the left *)
    inversion HMemCtrlm.
    subst.
    clear HMemCtrlm.
    subst.

    apply Bag.mem_in_to_list with (R:=eq) (E:=Equivalence_eq) in HIn.
    apply Bag.mem_split with (ED:=fromController_eqdec) in HIn.
    destruct HIn as [ctrlm0' HIn].

    (* TODO(arjun): The ConsistentDataLink invariants states that a link's
       source and destination is reflected in the topo function. We need
       another invariant stating that if the topo function is defined for
       a pair of locations, then there must exist a link between them. *)
    assert (exists pks, In (DataLink (swId0,p) pks (sw,pt)) links0) as X.
      admit.
    destruct X as [pks Hlink].
    apply in_split in Hlink.
    destruct Hlink as [links01 [links02 Hlink]].
    subst.

    assert 
      (LinkHasDst
         switches0
         (DataLink (swId0,p) pks (sw,pt))) as J0.
      apply linksHaveDst0...
    unfold LinkHasDst in J0.
    destruct J0 as [switch2 [HSw2In [HSw2IdEq HSw2PtsIn]]].
    destruct switch2.
    simpl in *.
    subst.
    remember (process_packet tbl1 pt pk) as X eqn:Hprocess.
    destruct X as [outp1' pktIns].

    eapply Bag.mem_equiv with (ED := switch_eqdec) in HSw2In.
    2: exact Xrel.
    destruct HSw2In as [switch1 [HMem2 Hswitch2]].
    apply Bag.mem_union in HMem2.
    destruct HMem2. 

    idtac "TODO(arjun): src and dst switches are the same".
    admit.

    apply Bag.mem_split with (ED := switch_eqdec) in H1.
    destruct H1 as [sws0 H1].
    rewrite -> H1 in Xrel.

    apply simpl_weak_sim with
      (devs2 := 
        State ({| Switch swId0 pts0 tbl0 inp0 outp0 ctrlm0' switchm0 |} <+>
               ({| Switch swId1 pts1 tbl1 
                          ((FromList (map (fun pk => (pt,pk)) pks)) <+> inp1) 
                          (FromList outp1' <+> outp1)
                        ctrlm1 (FromList (map (PacketIn pt) pktIns) <+> switchm1)|}) <+>
               sws0)
              (links01 ++ (DataLink (swId0,p) nil (swId1,pt)) :: links02)
              ofLinks0
              ctrl0).
    apply multistep_tau with
      (a0 :=
         (State (({|Switch swId0 pts0 tbl0 inp0 outp0 
                           (({|PacketOut p pk|}) <+> ctrlm0')
                           switchm0|}) <+>
                 ({|Switch swId1 pts1 tbl1 inp1 outp1 ctrlm1 switchm1|}) <+>
                 sws0)
                (links01 ++ (DataLink (swId0,p) pks (swId1,pt)) :: links02)
                ofLinks0
                ctrl0)).
    apply StepEquivState.
    apply StateEquiv.
    rewrite -> Xrel.
    apply Bag.pop_union.
    apply Bag.equiv_singleton.
    apply SwitchEquiv; try solve [ apply reflexivity | auto ].
    apply Bag.pop_union.
    apply Bag.equiv_singleton.
    apply symmetry...
    apply reflexivity.

    eapply multistep_tau.
    apply SendPacketOut.
    idtac "TODO(arjun): port exists".
    admit.
    eapply ObserveFromOutp... (* #winning *)
    
    unfold relate.
    simpl.
    rewrite -> H.
    apply reflexivity.
    trivial.

    (* ********************************************************************** *)
    (* Case 4 : Packet is in a PacketIn message                               *)
    (* ********************************************************************** *)

    idtac "TODO(arjun): PacketIn case".
    admit.

    (* ********************************************************************** *)
    (* Case 5 : Packet is on a data link                                      *)
    (* ********************************************************************** *)
    
    apply Bag.mem_unions_map in HMemLink.
    destruct HMemLink as [link [HIn HMemLink]].
    simpl in HIn.
    destruct link.
    destruct dst0 as [sw0 pt0].
    simpl in HMemLink.
    rewrite -> in_map_iff in HMemLink.
    destruct HMemLink as [pk0 [HEq HMemLink]].
    inversion HEq. subst. clear HEq.
    apply in_split in HMemLink.
    destruct HMemLink as [pks01 [pks02 HMemLink]].
    subst.

    assert
      (LinkHasDst 
         switches0 (DataLink src0 (pks01 ++ pk :: pks02) (sw,pt))) as J0.
      apply linksHaveDst0...
    unfold LinkHasDst in J0.
    destruct J0 as [switch2 [HSw2In [HSw2IdEq HSw2PtsIn]]].
    destruct switch2.
    simpl in *.
    subst.
    apply Bag.mem_split with (ED:=switch_eqdec) in HSw2In.
    destruct HSw2In as [sws HSw2In].
    remember (process_packet tbl0 pt pk) as X eqn:Hprocess.
    destruct X as [pktOuts pktIns].

    apply in_split in HIn.
    destruct HIn as [links01 [links02 HIn]].
    subst.
    
    apply simpl_weak_sim with
      (devs2 := 
        State (({| Switch swId0 pts0 tbl0
                          ((FromList (map (fun pk => (pt,pk)) pks02)) <+> inp0) 
                          (FromList pktOuts <+> outp0)
                          ctrlm0
                          (FromList (map (PacketIn pt) pktIns) <+> switchm0)|})
                <+> sws)
              (links01 ++ (DataLink src0 pks01 (swId0,pt)) :: links02)
              ofLinks0
              ctrl0).
    apply multistep_tau with
      (a0 := 
        State (({| Switch swId0 pts0 tbl0 inp0 outp0 ctrlm0 switchm0 |})
                <+> sws)
              (links01 ++ (DataLink src0 (pks01 ++ pk :: pks02)
                                    (swId0,pt)) :: links02)
              ofLinks0
              ctrl0).
    apply StepEquivState.
    apply StateEquiv.
    rewrite -> HSw2In.
    apply Bag.pop_union_l.
    apply reflexivity.
    assert ((pks01 ++ [pk]) ++ pks02 = pks01 ++ pk :: pks02) as X.
      rewrite <- app_assoc...
    rewrite <- X. clear X.
    destruct (SimpleDraimWire sws
      swId0 pts0 tbl0 inp0 outp0 ctrlm0 switchm0
      links01 src0 (pks01 ++ [pk]) pks02 swId0 pt links02 ofLinks0 ctrl0)
    as [inp4 step].
    eapply multistep_app with (obs2 := [(swId0,pt,pk)]).
    exact step.
    eapply multistep_tau.
    apply RecvDataLink.
    eapply multistep_obs.
    apply PktProcess.
      symmetry. exact Hprocess.
    admit. (* TODO(arjun): requires touchup to RecvDataLink. *)
    auto.
    rewrite -> H. apply reflexivity.
    trivial.

    (* ********************************************************************** *)
    (* Cases 6 and 7 : Packet is on an OpenFlow link                          *)
    (* ********************************************************************** *)
    
    simpl in HMemOFLink.
    simpl in *.
    apply Bag.mem_unions_map in HMemOFLink.
    destruct HMemOFLink as [link0 [HIn HMem]].
    destruct link0.
    simpl in HMem.
    destruct HMem as [HMemCtrlm | HMemSwitchm].

    (* ************************************************************************)
    (* Case 6 : Packet is in a PacketOut message from the controller          *)
    (* ************************************************************************)

    admit.

    (* ************************************************************************)
    (* Case 7 : Packet is in a PacketIn message from a switch                 *)
    (* ************************************************************************)

    admit.

    (* ************************************************************************)
    (* Case 8 : Packet is at the controller                                   *)
    (* ************************************************************************)

    admit.

Qed.



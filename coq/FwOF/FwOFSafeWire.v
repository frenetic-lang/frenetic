Set Implicit Arguments.

Require Import Coq.Lists.List.
Require Import Common.Types.
Require Import Common.Bisimulation.
Require Import Bag.TotalOrder.
Require Import Bag.Bag2.
Require Import FwOF.FwOFSignatures.
Require Import Common.Bisimulation.
Require Import Common.AllDiff.
Require FwOF.FwOFMachine.
Require FwOF.FwOFSimpleController.

Local Open Scope list_scope.
Local Open Scope bag_scope.

Module Make (Import Machine : MACHINE).
  Import Atoms.

  Inductive NotPacketOut : fromController -> Prop :=
  | BarrierRequest_NotPacketOut : forall xid,
                                    NotPacketOut (BarrierRequest xid)
  | FlowMod_NotPacketOut : forall fm,
                             NotPacketOut (FlowMod fm).

  Hint Constructors NotPacketOut NotFlowMod.

  Inductive Alternating : bool -> list fromController -> Prop :=
  | Alternating_Nil : forall b, Alternating b nil
  | Alternating_PacketOut : 
      forall b pt pk lst,
        Alternating b lst ->
        Alternating b (PacketOut pt pk :: lst)
  | Alternating_FlowMod : 
      forall f lst,
        Alternating true lst ->
        Alternating false (FlowMod f :: lst)
  | Alternating_BarrierRequest :
      forall b n lst,
        Alternating false lst ->
        Alternating b (BarrierRequest n :: lst).

  Inductive Approximating : switchId -> flowTable -> list fromController -> Prop :=
  | Approximating_Nil : 
      forall sw tbl, Approximating sw tbl nil
  | Approximating_FlowMod : 
      forall sw f tbl lst,
        FlowTableSafe sw (modify_flow_table f tbl) ->
        Approximating sw (modify_flow_table f tbl) lst ->
        Approximating sw tbl (lst ++ [FlowMod f])
  | Approximating_PacketOut :
      forall sw pt pk tbl lst,
        Approximating sw tbl lst ->
        Approximating sw tbl (lst ++ [PacketOut pt pk])
  | Approximating_BarrierRequest : 
      forall sw n tbl lst,
        Approximating sw tbl lst ->
        Approximating sw tbl (lst ++ [BarrierRequest n]).

  Inductive Barriered : switchId -> list fromController -> flowTable  -> bag fromController_le -> Prop :=
  | Barriered_NoFlowMods : 
      forall swId lst ctrlm tbl,
        (forall msg, In msg (to_list ctrlm) -> NotFlowMod msg) ->
        Alternating false lst ->
        Approximating swId tbl lst ->
        FlowTableSafe swId tbl ->
        Barriered swId lst tbl ctrlm
  | Barriered_OneFlowMod  : 
      forall swId lst ctrlm f tbl,
        (forall msg, In msg (to_list ctrlm) -> NotFlowMod msg) ->
        Alternating false (lst ++ [FlowMod f]) ->
        Approximating swId tbl (lst ++ [FlowMod f]) ->
        FlowTableSafe swId tbl ->
        Barriered swId lst tbl (({|FlowMod f|}) <+> ctrlm).

  Hint Constructors Alternating Approximating.
  
  (* We don't need alternating_push, if we precompute the sequence! *)
  Lemma alternating_pop : forall b x xs,
                            Alternating b (xs ++ [x]) -> Alternating b xs.
  Proof with auto with datatypes.
    intros b x xs H.
    generalize dependent b.
    induction xs; intros...
    simpl in H.
    inversion H...
  Qed.

  Lemma alternating_fm_fm_false :
    forall b lst f f0,
      Alternating b ((lst ++ [FlowMod f]) ++ [FlowMod f0]) ->
      False.
  Proof with eauto with datatypes.
    intros b lst f f0 H.
    generalize dependent b.
    induction lst; intros...
    + simpl in H.
      inversion H; subst...
      inversion H2.
    + simpl in H.
      inversion H; subst...
  Qed.

  Lemma approximating_pop_FlowMod : 
    forall sw tbl lst f,
      Approximating sw tbl (lst ++ [FlowMod f]) ->
      Approximating sw (modify_flow_table f tbl) lst.
  Proof with auto with datatypes.
    intros.
    inversion H; subst.
    + destruct lst; simpl in H3; inversion H3.
    + apply cons_tail in H0. destruct H0. inversion H2; subst...
    + apply cons_tail in H0. destruct H0. inversion H1...
    + apply cons_tail in H0. destruct H0. inversion H1...
  Qed.

  Lemma approximating_pop_BarrierRequest : 
    forall sw tbl lst n,
      Approximating sw tbl (lst ++ [BarrierRequest n]) ->
      Approximating sw tbl lst.
  Proof with auto with datatypes.
    intros.
    inversion H; subst.
    + destruct lst; simpl in H3; inversion H3.
    + apply cons_tail in H0. destruct H0. inversion H2.
    + apply cons_tail in H0. destruct H0. inversion H1.
    + apply cons_tail in H0. destruct H0. inversion H1; subst...
  Qed.

  Lemma approximating_pop_PacketOut : 
    forall sw tbl lst pt pk,
      Approximating sw tbl (lst ++ [PacketOut pt pk]) ->
      Approximating sw tbl lst.
  Proof with auto with datatypes.
    intros.
    inversion H; subst.
    + destruct lst; simpl in H3; inversion H3.
    + apply cons_tail in H0. destruct H0. inversion H2.
    + apply cons_tail in H0. destruct H0. inversion H1; subst...
    + apply cons_tail in H0. destruct H0. inversion H1.
  Qed.
  
  Lemma approximating_pop_FlowMod_safe : 
    forall sw tbl lst f,
      Approximating sw tbl (lst ++ [FlowMod f]) ->
      FlowTableSafe sw (modify_flow_table f tbl).
  Proof with auto with datatypes.
    intros sw tbl lst f H.
    inversion H; subst.
    + destruct lst; simpl in H3; inversion H3.
    + apply cons_tail in H0. destruct H0. inversion H2; subst...
    + apply cons_tail in H0. destruct H0. inversion H1...
    + apply cons_tail in H0. destruct H0. inversion H1...
  Qed.

  Lemma Barriered_entails_FlowModSafe : 
    forall swId lst tbl ctrlm,
      Barriered swId lst tbl ctrlm ->
      FlowModSafe swId tbl ctrlm.
  Proof with eauto with datatypes.
    intros.
    inversion H; subst...
    + eapply NoFlowModsInBuffer...
    + eapply OneFlowModsInBuffer...
      inversion H2; subst...
    - destruct lst; simpl in H7; inversion H7.
    - apply cons_tail in H4.
      destruct H4; subst.
      inversion H6; subst...
    - apply cons_tail in H4.
      destruct H4.
      inversion H5.
    - apply cons_tail in H4.
      destruct H4.
      inversion H5.
  Qed.

  Lemma barriered_pop_BarrierRequest : 
    forall swId xid lst tbl ctrlm,
      Barriered swId (lst ++ [BarrierRequest xid]) tbl ctrlm ->
      (forall msg, In msg (to_list ctrlm) -> NotFlowMod msg) ->
      Barriered swId lst tbl ctrlm.
  Proof with eauto with datatypes.
    intros.
    rename H0 into X.
    inversion H; subst.
    + apply Barriered_NoFlowMods...
      apply alternating_pop in H1...
      apply approximating_pop_BarrierRequest in H2...
    + assert (NotFlowMod (FlowMod f)).
      apply X.
      apply Bag.in_union; simpl...
      inversion H4.
  Qed.
  
  Lemma alternating_splice_PacketOut :
    forall b lst1 pt pk lst2,
      Alternating b (lst1 ++ PacketOut pt pk :: lst2) <->
      Alternating b (lst1 ++ lst2).
  Proof with auto with datatypes.
    intros b lst1 pt pk lst2.
    split.
    + intros H.
      generalize dependent b.
      induction lst1; intros; simpl in *; inversion H...
    + intros H.
      generalize dependent b.
      induction lst1; intros...
    - simpl in *...
               - simpl in *.
      inversion H; subst...
  Qed.
  
  Hint Resolve alternating_pop approximating_pop_PacketOut approximating_pop_FlowMod
       approximating_pop_BarrierRequest.
  
  Lemma approximating_splice_PacketOut : 
    forall sw tbl lst1 pt pk lst2,
      Approximating sw tbl (lst1 ++ PacketOut pt pk :: lst2) <->
      Approximating sw tbl (lst1 ++ lst2).
  Proof with eauto with datatypes.
    intros sw tbl lst1 pt pk lst2.
    split.
    + intros H.
      generalize dependent tbl.
      induction lst2 using rev_ind; intros.
    - rewrite -> app_nil_r...
    - rewrite -> app_comm_cons in H.
      rewrite -> app_assoc in H.
      rewrite -> app_assoc.
      destruct x...
      assert (FlowTableSafe sw (modify_flow_table f tbl0)).
      { eapply approximating_pop_FlowMod_safe... }
      eauto.
      + intros H.
        generalize dependent tbl.
        induction lst2 using rev_ind; intros.
    - rewrite -> app_nil_r in H...
    - rewrite -> app_comm_cons.
      rewrite -> app_assoc.
      rewrite -> app_assoc in H.
      destruct x...
      assert (FlowTableSafe sw (modify_flow_table f tbl0)) as X...
      { eapply approximating_pop_FlowMod_safe... }
        Grab Existential Variables.
      exact 0.
  Qed.

  Hint Resolve alternating_splice_PacketOut approximating_splice_PacketOut.
  
  Lemma barriered_pop_PacketOut :
    forall sw pt pk lst tbl ctrlm,
      Barriered sw (lst ++ [PacketOut pt pk]) tbl ctrlm ->
      Barriered sw lst tbl (({|Atoms.PacketOut pt pk|}) <+> ctrlm).
  Proof with eauto with datatypes.
    intros sw pt pk lst tbl ctrlm H.
    inversion H; subst.
    + apply Barriered_NoFlowMods...
    - intros.
      apply Bag.in_union in H4; simpl in H4.
      destruct H4 as [[H4 | H4] | H4]; subst...
      inversion H4.
      + rewrite <-  Bag.union_assoc.
        rewrite <- (Bag.union_comm _ ({|FlowMod f|})).
        rewrite -> Bag.union_assoc.
        apply Barriered_OneFlowMod...
    - intros.
      apply Bag.in_union in H4; simpl in H4.
      destruct H4 as [[H4 | H4] | H4]; subst...
      inversion H4.
    - rewrite <- app_assoc in H1.
      simpl in H1.
      apply alternating_splice_PacketOut in H1...
    - rewrite <- app_assoc in H2.
      simpl in H2.
      apply approximating_splice_PacketOut in H2...
      Grab Existential Variables. exact 0.
  Qed.

  Lemma barriered_splice_PacketOut :
    forall sw lst1 pt pk lst2 tbl ctrlm,
      Barriered sw (lst1 ++ lst2) tbl ctrlm ->
      Barriered sw (lst1 ++ PacketOut pt pk :: lst2) tbl ctrlm.
  Proof with eauto with datatypes.
    intros sw lst1 pt pk lst2 tbl ctrlm H.
    inversion H; subst.
    + eapply Barriered_NoFlowMods...
    - eapply alternating_splice_PacketOut...
    - eapply approximating_splice_PacketOut...
      + eapply Barriered_OneFlowMod...
    - rewrite <- app_assoc.
      rewrite <- app_comm_cons.
      apply alternating_splice_PacketOut...
      rewrite -> app_assoc...
    - rewrite <- app_assoc.
      rewrite <- app_comm_cons.
      apply approximating_splice_PacketOut...
      rewrite -> app_assoc...
  Qed.

  Lemma barriered_process_PacketOut : 
    forall sw lst tbl pt pk ctrlm,
      Barriered sw lst tbl (({|PacketOut pt pk|}) <+> ctrlm) ->
      Barriered sw lst tbl ctrlm.
  Proof with eauto with datatypes.
    intros.
    inversion H; subst.
    + eapply Barriered_NoFlowMods...
      intros. apply H0. apply Bag.in_union...
    + apply Bag.union_from_ordered in H0.
      assert (In (FlowMod f) (to_list ctrlm0)) as J.
      { assert (In (FlowMod f) (to_list (({|PacketOut pt pk|}) <+> ctrlm0))) as J.
        rewrite <- H0. apply Bag.in_union; simpl...
        apply Bag.in_union in J. simpl in J.
        destruct J as [[J|J]|J]...
        + inversion J.
        + inversion J. }
      eapply Bag.in_split in J.
      destruct J as [ctrlm2 HEq].
      rewrite -> HEq.
      eapply Barriered_OneFlowMod...
      intros.
      subst.
      rewrite <- Bag.union_assoc in H0.
      rewrite -> (Bag.union_comm _ ({|PacketOut pt pk|})) in H0.
      rewrite -> Bag.union_assoc in H0.
      apply Bag.pop_union_l in H0.
      subst.
      eapply H1.
      apply Bag.in_union...
  Qed.

  Lemma barriered_pop_FlowMod : forall sw f tbl lst ctrlm,
                                  (forall x, In x (to_list ctrlm) -> NotFlowMod x) ->
                                  Barriered sw (lst ++ [FlowMod f]) tbl ctrlm ->
                                  Barriered sw lst tbl (({|FlowMod f|}) <+> ctrlm).
  Proof with eauto with datatypes.
    intros sw f tbl lst ctrlm H H0.
    inversion H0; subst.
    + apply Barriered_OneFlowMod...
    + assert (NotFlowMod (FlowMod f0)) as X.
      apply H. apply Bag.in_union; simpl...
      inversion X.
  Qed.

End Make.

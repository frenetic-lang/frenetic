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
  Module Concrete := Relation.Concrete.

  Import Relation.
  Import Relation.Concrete.

  Lemma LinksHaveSrc_untouched : forall 
    {swId tbl pts sws sws0 links
    inp outp ctrlm switchm tbl' inp' outp' ctrlm' switchm' },
    LinksHaveSrc 
      (sws ++ (Switch swId pts tbl inp outp ctrlm switchm) :: sws0)  links ->
    LinksHaveSrc 
      (sws ++ (Switch swId pts tbl' inp' outp' ctrlm' switchm') :: sws0)
      links.
  Admitted.

  Lemma LinksHaveDst_untouched : forall 
    {swId tbl pts sws sws0 links
    inp outp ctrlm switchm tbl' inp' outp' ctrlm' switchm' },
    LinksHaveDst
      (sws ++ (Switch swId pts tbl inp outp ctrlm switchm) :: sws0)  links ->
    LinksHaveDst 
      (sws ++ (Switch swId pts tbl' inp' outp' ctrlm' switchm') :: sws0)
      links.
  Admitted.

   Lemma LinkTopoOK_inv : forall {links links0 src dst} pks pks',
     ConsistentDataLinks (links ++ (DataLink src pks dst) :: links0) ->
     ConsistentDataLinks (links ++ (DataLink src pks' dst) :: links0).
   Proof with auto with datatypes.
     intros.
     unfold ConsistentDataLinks in *.
     intros.
     apply in_app_iff in H0.
     simpl in H0.
     destruct H0 as [H0 | [H0 | H0]]...
     pose (lnk' := (DataLink src0 pks0 dst0)).
     remember (H lnk').
     assert (In lnk' (links0 ++ lnk' :: links1))...
     apply e in H1.
     simpl in H1.
     inversion H0.
     simpl...
   Qed.

  Lemma FlowTablesSafe_equiv: forall 
    { sw sw' sws sws0 },
    sw === sw' ->
    FlowTablesSafe (sws ++ sw :: sws0) ->
    FlowTablesSafe (sws ++ sw' :: sws0).
  Proof with eauto with datatypes.
    intros.
    unfold FlowTablesSafe in *.
    intros.
    apply in_app_iff in H1.
    simpl in H1.
    destruct H1 as [HIn | [HIn | HIn]].
    eapply H0...
    (* Detailed case. *)
    unfold Equivalence.equiv in H.
    destruct H.
    inversion HIn.
    subst.
    clear HIn.
    remember (H0 swId0 pts0 tbl0 inp1 outp1 ctrlm1 switchm1) as X.
    clear HeqX H0.
    apply X...
    eapply H0...
  Qed.

  Lemma FlowTablesSafe_untouched : forall {sws sws0 swId pts tbl inp inp'
    outp outp' ctrlm ctrlm' switchm switchm' },
    FlowTablesSafe (sws++(Switch swId pts tbl inp outp ctrlm switchm)::sws0) ->
    FlowTablesSafe 
      (sws++(Switch swId pts tbl inp' outp' ctrlm' switchm')::sws0).
  Proof with eauto.
    intros.
  Admitted.

  Lemma LinkHasSrc_equiv : forall {sws sws' link},
    sws === sws' ->
    LinkHasSrc sws link ->
    LinkHasSrc sws' link.
  Proof with auto with datatypes.
    intros.
    destruct link.
    inversion H0.
    destruct src.
    simpl in H1.
    destruct H1 as [HInSw [HSwEq HInPt]].
    destruct x; simpl in *; subst.
    unfold Equivalence.equiv in H.
    unfold LinkHasSrc in *.
    destruct H0 as [switch0 [HIn [HSwEq HInPt0]]].
    simpl in *.
    generalize dependent sws'.
    induction sws.
    (* Contradiction *)
    intros. destruct sws'. inversion HInSw. simpl in H. contradiction.
    (* Outer inductive case. *)
    intros. destruct sws'. simpl in H. contradiction.
    (* Outer + inner induction *)
    simpl in H.
    destruct H as [H Hrec].
    simpl in HInSw.
    destruct HInSw as [HInSw | HInSw];
    destruct HIn as [HIn | HIn].
    (* Case 1 *)
    subst.
    rewrite -> HInSw in *.
    simpl in *.
    exists s.
    split...
    destruct s.
    unfold Equivalence.equiv in H.
    inversion H.
    subst.
    simpl.
    split...
    (* Case 2 *)
    clear IHsws.
    admit. (* requires unique IDs *)
    (* Case 3 *)
    admit. (* requires unique IDs *)
    (* Case 4 *)
    remember (IHsws HIn HInSw sws' Hrec) as J eqn:X.
    clear IHsws X.
    destruct J as [theSwitch0 [HIn0 [HSwEq0 HPtsIn0]]].
    exists theSwitch0...
  Qed.

  Lemma LinksHaveSrc_inv : forall {sws links links0 src dst} pks pks',
    LinksHaveSrc sws (links ++ (DataLink src pks dst) :: links0) ->
    LinksHaveSrc sws (links ++ (DataLink src pks' dst) :: links0).
  Proof with auto with datatypes.
  Admitted.

  Lemma LinksHaveDst_inv : forall {sws links links0 src dst} pks pks',
    LinksHaveDst sws (links ++ (DataLink src pks dst) :: links0) ->
    LinksHaveDst sws (links ++ (DataLink src pks' dst) :: links0).
  Admitted.

End Make.
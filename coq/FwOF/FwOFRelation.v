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


Ltac bag_perm n:=
  match goal with
    | |- ?bag === ?bag => 
      idtac "SOLVED.";
      apply reflexivity
    | |- ?b <+> ?lst === ?b <+> ?lst0 =>
      let newn := eval compute in (Bag.depth lst) in
        idtac "popped" b "now solving" lst "and" lst0;
        apply Bag.pop_union_l;
          bag_perm newn
    | |- ?b <+> ?lst1  === ?lst2 =>
      match eval compute in n with
        | O => idtac "failed"; fail "out of time / not equivalent"
        | _ => idtac "Rotating: " b "<+>" lst1 "===" lst2;
          apply Bag.rotate_union;
            repeat rewrite -> Bag.union_assoc;
              bag_perm (pred n)
      end
  end.

Ltac solve_bag_permutation :=
  bag_perm 100.

Example solve_bag_perm_example1 : forall (A : Type) (E: Eq A)
  (b0 b1 b2 : Bag.bag A),
  b0 <+> b1 <+> b2 === b0 <+> b1 <+> b2.
Proof.
  intros.
  solve_bag_permutation.
Qed.

Example solve_bag_perm_example2 : forall (A : Type) (E: Eq A)
  (b0 b1 b2 : Bag.bag A),
  b0 <+> b1 <+> b2 === b1 <+> b2 <+> b0.
Proof.
  intros.
  solve_bag_permutation.
Qed.


Example solve_bag_perm_example3 : forall (A : Type) (E: Eq A)
  (b0 b1 b2 : Bag.bag A),
  b0 <+> b1 <+> b2 === b1 <+> b0 <+> b2.
Proof.
  intros.
  solve_bag_permutation.
Qed.

Example solve_bag_perm_example4 : forall (A : Type) (E: Eq A)
  (b0 b1 b2 b3 b4 b5 b6: Bag.bag A),
  b3 <+> b0 <+> b5 <+> b1 <+> b4 <+> b2 <+> b6 === 
  b1 <+> b4 <+> b5 <+> b6 <+> b3 <+> b0 <+> b2.
Proof.
  intros.
   bag_perm 100.
Qed.

Example solve_bag_perm_example5 : forall (A : Type) (E: Eq A)
  (b0 b1 b2 : Bag.bag A),
  b0 <+> b2 === b1 <+> b2.
Proof.
  intros.
Admitted.


Module Make (Import Atoms : ATOMS).

  Module Concrete := ConcreteSemantics (Atoms).
  Import Concrete.

  Axiom topo : switchId -> portId -> option (switchId * portId).

  Definition affixSwitch (sw : switchId) (ptpk : portId * packet) :=
    match ptpk with
      | (pt,pk) => (sw,pt,pk)
    end.

  Definition transfer (sw : switchId) (ptpk : portId * packet) :=
    match ptpk with
      | (pt,pk) =>
        match topo sw pt with
          | Some (sw',pt') => {| (sw',pt',pk) |}
          | None => {| |}
        end
    end.

  Definition select_packet_out (sw : switchId) (msg : fromController) :=
    match msg with
      | PacketOut pt pk => {| (sw,pt,pk) |}
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

  Definition abst_state := Bag.bag (switchId * portId * packet).

  Axiom abst_func : switchId -> portId -> packet -> list (portId * packet).

  Definition flow_table_safe (sw : switchId) (tbl : flowTable) : Prop :=
    forall pt pk forwardedPkts packetIns,
      process_packet tbl pt pk = (forwardedPkts, packetIns) ->
      Bag.unions (map (transfer sw) forwardedPkts) <+>
      Bag.unions (map (select_packet_in sw) (map (PacketIn pt) packetIns)) ===
      Bag.unions (map (transfer sw) (abst_func sw pt pk)).

  Definition state_switches (st : state) := 
    match st with
      | (switches, _, _, _) => switches
    end.

  Record concreteState := ConcreteState {
    concreteState_state : state;
    concreteState_flowTableSafety :
      forall (sw : switch), 
        In sw (state_switches concreteState_state) -> 
        flow_table_safe (switch_swichId sw) (switch_flowTable sw)
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

  Axiom relate_controller : controller -> abst_state.

  Definition relate (st : state) : abst_state :=
    match st with
      | (switches, links, ofLinks, ctrl) =>
        Bag.unions (map relate_switch switches) <+>
        Bag.unions (map relate_dataLink links) <+>
        Bag.unions (map relate_openFlowLink ofLinks) <+>
        relate_controller ctrl
    end.
        
  Definition bisim_relation : relation concreteState abst_state :=
    fun (st : concreteState) (ast : abst_state) => 
      ast === (relate (concreteState_state st)).

  Axiom unions_unlist_2 : forall (A B : Type) (EA : Eq A) (EB : Eq B)
    (f : A -> Bag.bag B) (lst : list A) (bag : Bag.bag A),
    Bag.unions (map f (lst ++ Bag.to_list bag)) ===
    (Bag.unions (map f lst)) <+> (Bag.unions (map f (Bag.to_list bag))).
    
  Theorem weak_sim_1 :
    weak_simulation concreteStep abstractStep bisim_relation.
  Proof with auto with datatypes.
    unfold weak_simulation.
    intros.
    split.
    intros.
    unfold bisim_relation in H.
    unfold concreteStep in H0.
    destruct s. destruct s'. simpl in *.
    inversion H0; subst.
    inversion H1; subst.
    simpl in H.
    rewrite -> map_app in H.
    simpl in H.
    rewrite -> Bag.unions_app in H.
    simpl in H.
    rewrite -> Bag.from_list_cons in H.
    repeat rewrite -> Bag.union_assoc in H.
    rewrite -> Bag.union_comm in H.
    rewrite -> Bag.union_assoc in H.
    match goal with
      | [ H : t === ?t0 <+> ?t1 |- _ ] => remember t1
    end.
    remember (Bag.unions (map (transfer swId) (abst_func swId pt pk)) <+> b) as t'.
    exists t'.
    split.
    Focus 2.
    apply multistep_tau with (a0 := ({|(swId, pt, pk)|}) <+> b).
    apply AbstractStepEquiv...
    apply multistep_obs with
      (a0 := (Bag.unions (map (transfer swId) (abst_func swId pt pk)) <+> b)).
    apply AbstractStep.
    subst.
    apply multistep_nil.
    (* Showing the states are bisimilar *)
    unfold bisim_relation.
    simpl.
    rewrite -> map_app.
    rewrite -> Bag.unions_app.
    simpl.
    clear H0. clear H1.
    rewrite -> unions_unlist_2.
    rewrite -> unions_unlist_2.
    repeat rewrite -> Bag.union_assoc.
    unfold state_switches in *.
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
    solve_bag_permutation.
    match goal with
      | |- ?X === ?Y => make_big_Bag.unions X; make_big_Bag.unions Y
    end.
    repeat rewrite -> flatten_Bag.unions_r.

    match goal with
      | |- ?X === ?Y => flatten_nested_Bag.unions X
    end.

    flatten_nested_Bag.unions .


    rw (  Bag.unions (map (transfer swId) outp') <+>
   Bag.unions (map (select_packet_in swId) (map (PacketIn pt) pksToCtrl)) <+>
   Bag.FromList (map (affixSwitch swId) (Bag.to_list inp)) <+>
   Bag.unions (map (transfer swId) (Bag.to_list outp)) <+>
   Bag.unions (map (select_packet_out swId) (Bag.to_list ctrlm)) <+>
   Bag.unions (map (select_packet_in swId) (Bag.to_list switchm)) <+>
   Bag.unions (map relate_switch sws0) <+>
   Bag.unions (map relate_dataLink links) <+>
   Bag.unions (map relate_openFlowLink ofLinks) <+>
   relate_controller ctrl <+> Bag.unions (map relate_switch sws)).
    rwlhs (Bag.unions (map relate_dataLink links)).
          
     rewrite -> (make_Bag.unions _ ).
    Check make_Bag.unions.
    Axiom flatten_Bag.unions : forall (A : Type) (E : Eq A)
      

    admit. (* gotta bubble sort ... *)
    admit.
    admit.
    admit.
    
    (* No observations *)
    intros.

  Admitted.
    
  Theorem weak_sim_2 :
    weak_simulation abstractStep concreteStep (inverse_relation bisim_relation).
  Proof with auto.

  Admitted.


  Theorem fwof_abst_weak_bisim :
    weak_bisimulation concreteStep abstractStep bisim_relation.
  Proof.
    unfold weak_bisimulation.
    split.
    exact weak_sim_1.
    exact weak_sim_2.
  Qed.

End Make.    
  

  

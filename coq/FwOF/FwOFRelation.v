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

Module Make (Import Atoms : ATOMS).

  Module Concrete := ConcreteSemantics (Atoms).
  Import Concrete.

  Axiom topo : switchId * portId -> option (switchId * portId).

  Definition abst_state := Bag.bag (switchId * portId * packet).

  Axiom relate_controller : controller -> abst_state.

  Axiom abst_func : switchId -> portId -> packet -> list (portId * packet).

  Definition affixSwitch (sw : switchId) (ptpk : portId * packet) :=
    match ptpk with
      | (pt,pk) => (sw,pt,pk)
    end.

  Definition transfer (sw : switchId) (ptpk : portId * packet) :=
    match ptpk with
      | (pt,pk) =>
        match topo (sw,pt) with
          | Some (sw',pt') => {| (sw',pt',pk) |}
          | None => {| |}
        end
    end.

  Definition select_packet_out (sw : switchId) (msg : fromController) :=
    match msg with
      | PacketOut pt pk => transfer sw (pt,pk)
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

  Definition flow_table_safe (sw : switchId) (tbl : flowTable) : Prop :=
    forall pt pk forwardedPkts packetIns,
      process_packet tbl pt pk = (forwardedPkts, packetIns) ->
      Bag.unions (map (transfer sw) forwardedPkts) <+>
      Bag.unions (map (select_packet_in sw) (map (PacketIn pt) packetIns)) ===
      Bag.unions (map (transfer sw) (abst_func sw pt pk)).

  Definition ConsistentDataLinks (st : state) : Prop :=
    forall (lnk : dataLink),
      In lnk (state_dataLinks st) ->
      topo (dataLink_src lnk) = Some (dataLink_dst lnk).

  Axiom ControllerRemembersPackets :
    forall (ctrl ctrl' : controller),
      controller_step ctrl ctrl' ->
      relate_controller ctrl = relate_controller ctrl'.

  Axiom ControllerSendForgetsPackets : forall ctrl ctrl' sw msg,
    controller_send ctrl ctrl' sw msg ->
    relate_controller ctrl === select_packet_out sw msg <+>
    relate_controller ctrl'.

  Axiom ControllerRecvRemembersPackets : forall ctrl ctrl' sw msg,
    controller_recv ctrl sw msg ctrl' ->
    relate_controller ctrl' === select_packet_in sw msg <+> 
    (relate_controller ctrl).
    
  Record concreteState := ConcreteState {
    concreteState_state : state;
    concreteState_flowTableSafety :
      forall (sw : switch), 
        In sw (state_switches concreteState_state) -> 
        flow_table_safe (switch_swichId sw) (switch_flowTable sw);
    concreteState_consistentDataLinks :
      ConsistentDataLinks concreteState_state
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


  Definition relate (st : state) : abst_state :=
    Bag.unions (map relate_switch (state_switches st)) <+>
    Bag.unions (map relate_dataLink (state_dataLinks st)) <+>
    Bag.unions (map relate_openFlowLink (state_openFlowLinks st)) <+>
    relate_controller (state_controller st).

  Definition bisim_relation : relation concreteState abst_state :=
    fun (st : concreteState) (ast : abst_state) => 
      ast === (relate (concreteState_state st)).

End Make.

(*
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
    Axiom Mem_unions : forall (A : Type) (E : Eq A) (x : A) lst, 
      Bag.Mem x (Bag.unions lst) ->
      exists elt, In elt lst /\ Bag.Mem x elt.
    apply Mem_unions in X.
    destruct X as [switch_abst [ Xin Xmem ]].
    rewrite -> in_map_iff in Xin.
    destruct Xin as [switch [Xrel Xin]].
    subst.
    destruct switch.
    simpl in Xmem.
    destruct Xmem as [Xmem | [Xmem | Xmem]].
    (* The packet is in the input buffer *)
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
  

*)  

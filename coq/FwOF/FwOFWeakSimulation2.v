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
    (* The packet is in the input buffer. *)
    (* Need to get switch_inputPackets = l1 ++ (pt,pk) :: l2 *)
    rewrite -> in_map_iff in Xmem.
    destruct Xmem as [ ptpk [XmemEq Xmem]].
    apply in_split in Xmem.
    destruct Xmem as [pks [pks0 Xmem]].
    apply in_split in Xin.
    destruct Xin as [sws [sws0 Xin]].
    pose (Y := process_packet switch_flowTable0 pt pk).
    destruct Y as [ newPks pktIns ].
    exists
      (@ConcreteState
        (State 
          (sws ++ 
            (Switch switch_swichId0 switch_ports0 switch_flowTable0
              ((Bag.FromList pks) <+> (Bag.FromList pks0))
              (Bag.FromList newPks <+> switch_outputPackets0)
              switch_fromController0
              (Bag.FromList (map (PacketIn pt)  pktIns) <+> switch_fromSwitch0))
            :: sws0)
          state_dataLinks0
          state_openFlowLinks0
          state_controller0)
        concreteState_flowTableSafety0
        concreteState_consistentDataLinks0).
) :: 
    Check in_map_iff.
    
  Admitted.

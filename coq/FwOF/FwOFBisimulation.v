Set Implicit Arguments.

Require Import Coq.Classes.Equivalence.
Require Import Common.Bisimulation.
Require Import FwOF.FwOF.
Require FwOF.FwOFRelationLemmas.
Require FwOF.FwOFWeakSimulation1.
Require FwOF.FwOFWeakSimulation2.

Local Open Scope equiv_scope.

Module Make (AtomsAndController : ATOMS_AND_CONTROLLER).

  Module WeakSim2 := FwOF.FwOFWeakSimulation2.Make 
                       (AtomsAndController) 
                       (FwOF.FwOFRelationLemmas.Make).
  Module WeakSim1 := WeakSim2.WeakSim1. 

  Import WeakSim1.RelationLemmas.Relation.

  Theorem fwof_abst_weak_bisim :
    weak_bisimulation concreteStep abstractStep bisim_relation.
  Proof.
    unfold weak_bisimulation.
    split.
    exact WeakSim1.weak_sim_1.
    exact WeakSim2.weak_sim_2.
  Qed.

End Make.    

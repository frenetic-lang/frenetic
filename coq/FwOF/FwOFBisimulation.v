Set Implicit Arguments.

Require Import Coq.Classes.Equivalence.
Require Import Common.Bisimulation.
Require Import FwOF.FwOFSignatures.
Require FwOF.FwOFRelationDefinitions.
Require FwOF.FwOFWellFormedness.
Require FwOF.FwOFWeakSimulation1.
Require FwOF.FwOFWeakSimulation2.

Local Open Scope equiv_scope.

Module Make (AtomsAndController : ATOMS_AND_CONTROLLER).

  Module RelationDefinitions := FwOF.FwOFRelationDefinitions.Make (AtomsAndController).
  Module Relation := FwOF.FwOFWellFormedness.Make (RelationDefinitions).
  Module WeakSim1 := FwOF.FwOFWeakSimulation1.Make (RelationDefinitions).
  Module WeakSim2 := FwOF.FwOFWeakSimulation2.Make (Relation).

  Import Relation.
  Import RelationDefinitions.

  Theorem fwof_abst_weak_bisim :
    weak_bisimulation concreteStep abstractStep bisim_relation.
  Proof.
    unfold weak_bisimulation.
    split.
    exact WeakSim1.weak_sim_1.
    exact WeakSim2.weak_sim_2.
  Qed.

End Make.

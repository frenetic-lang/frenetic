Set Implicit Arguments.

Require Import Coq.Classes.Equivalence.
Require Import Common.Bisimulation.
Require Import FwOF.FwOFSignatures.
Require FwOF.FwOFWellFormedness.
Require FwOF.FwOFWeakSimulation1.
Require FwOF.FwOFWeakSimulation2.

Local Open Scope equiv_scope.

Module Make (AtomsAndController : ATOMS_AND_CONTROLLER).

  Module WellFormedness := FwOF.FwOFWellFormedness.Make (AtomsAndController).
  Module WeakSim1 := FwOF.FwOFWeakSimulation1.Make (WellFormedness).
  Module WeakSim2 := FwOF.FwOFWeakSimulation2.Make (WellFormedness).

  Import WellFormedness.

  Theorem fwof_abst_weak_bisim :
    weak_bisimulation concreteStep abstractStep bisim_relation.
  Proof.
    unfold weak_bisimulation.
    split.
    exact WeakSim1.weak_sim_1.
    exact WeakSim2.weak_sim_2.
  Qed.

End Make.
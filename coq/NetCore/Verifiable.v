Set Implicit Arguments.

Require Import Coq.Lists.List.
Require Import OpenFlow.OpenFlow0x01Types.
Require Import NetCore.NetCoreEval.

Inductive Vf_act : act -> Prop :=
  | Vf_FwdUnmodifiedPhysicalPort : forall pt,
     Vf_act (Forward unmodified (PhysicalPort pt)).

Inductive Vf_pol : pol -> Prop :=
  | Vf_PoAtom : forall pr acts,
    (forall act, In act acts -> Vf_act act) ->
    Vf_pol (PoAtom pr acts)
  | Vf_PoUnion : forall pol1 pol2,
    Vf_pol pol1 ->
    Vf_pol pol2 ->
    Vf_pol (PoUnion pol1 pol2).

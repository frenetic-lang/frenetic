Set Implicit Arguments.

Require Import Coq.Lists.List.
Require Import OpenFlow.OpenFlow0x01Types.
Require Import NetCore.NetCoreEval.

Inductive Vf_act : act -> Prop :=
  | Vf_FwdUnmodifiedPhysicalPort : 
      forall pts,
      (forall pt,
         In pt pts -> exists n, pt = PhysicalPort n) ->
     Vf_act (Act unmodified pts nil).

Inductive Vf_pol : pol -> Prop :=
  | Vf_PoAtom : forall pr act,
    Vf_act act ->
    Vf_pol (PoAtom pr act)
  | Vf_PoUnion : forall pol1 pol2,
    Vf_pol pol1 ->
    Vf_pol pol2 ->
    Vf_pol (PoUnion pol1 pol2).

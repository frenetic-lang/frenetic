Set Implicit Arguments.

Require Import Coq.Lists.List.
Require Import Coq.Relations.Relations.
Require Import Common.Types.
Require Import Bag.TotalOrder.
Require Import Bag.Bag2.
Require Import Common.AllDiff.
Require Import Common.Bisimulation.
Require Import FwOF.FwOFSignatures.

Local Open Scope list_scope.
Local Open Scope equiv_scope.
Local Open Scope bag_scope.

(** This is a really trivial functor. RELATION_DEFINITIONS is just a bunch of definitions. *)
Module Make (Import AtomsAndController : ATOMS_AND_CONTROLLER) <: RELATION_DEFINITIONS.

  Import AtomsAndController.
  Import Machine.
  Import Atoms.

  Definition affixSwitch (sw : switchId) (ptpk : portId * packet) :=
    match ptpk with
      | (pt,pk) => (sw,pt,pk)
    end.

  Definition ConsistentDataLinks (links : list dataLink) : Prop :=
    forall (lnk : dataLink),
      In lnk links ->
      topo (src lnk) = Some (dst lnk).

  Definition LinkHasSrc (sws : bag switch_le) (link : dataLink) : Prop :=
    exists switch,
      In switch (to_list sws) /\
      fst (src link) = swId switch /\
      In (snd (src link)) (pts switch).

  Definition LinkHasDst (sws : bag switch_le) (link : dataLink) : Prop :=
    exists switch,
      In switch (to_list sws) /\
      fst (dst link) = swId switch /\
      In (snd (dst link)) (pts switch).

  Definition LinksHaveSrc (sws : bag switch_le) (links : list dataLink) :=
    forall link, In link links -> LinkHasSrc sws link.

  Definition LinksHaveDst (sws : bag switch_le) (links : list dataLink) :=
    forall link, In link links -> LinkHasDst sws link.

  Definition UniqSwIds (sws : bag switch_le) := AllDiff swId (to_list sws).

  Definition ofLinkHasSw (sws : bag switch_le) (ofLink : openFlowLink) :=
    exists sw,
      In sw (to_list sws) /\
      of_to ofLink = swId sw.

  Definition OFLinksHaveSw (sws : bag switch_le) (ofLinks : list openFlowLink) :=
    forall ofLink, In ofLink ofLinks -> ofLinkHasSw sws ofLink.

  Definition DevicesFromTopo (devs : state) :=
    forall swId0 swId1 pt0 pt1,
      Some (swId0,pt0) = topo (swId1,pt1) ->
      exists sw0 sw1 lnk,
        (* TODO(arjun): might as well be lists now. *)
        In sw0 (to_list (switches devs)) /\ 
        In sw1 (to_list (switches devs)) /\
        In lnk (links devs) /\
        swId sw0 = swId0 /\
        swId sw1 = swId1 /\
        src lnk = (swId1,pt1) /\
        dst lnk = (swId0, pt0).

  Definition NoBarriersInCtrlm (sws : bag switch_le) :=
    forall sw,
      In sw (to_list sws) ->
      forall m,
        In m (to_list (ctrlm sw)) ->
        NotBarrierRequest m.


  Record concreteState := ConcreteState {
    devices : state;
    concreteState_flowTableSafety : FlowTablesSafe (switches devices);
    concreteState_consistentDataLinks : ConsistentDataLinks (links devices);
    linksHaveSrc : LinksHaveSrc (switches devices) (links devices);
    linksHaveDst : LinksHaveDst (switches devices) (links devices);
    uniqSwIds : UniqSwIds (switches devices);
    ctrlP : P (switches devices) (ofLinks devices) (ctrl devices);
    uniqOfLinkIds : AllDiff of_to (ofLinks devices);
    ofLinksHaveSw : OFLinksHaveSw (switches devices) (ofLinks devices);
    devicesFromTopo : DevicesFromTopo devices;
    swsHaveOFLinks : SwitchesHaveOpenFlowLinks (switches devices) (ofLinks devices);
    noBarriersInCtrlm : NoBarriersInCtrlm (switches devices)
  }.

  Implicit Arguments ConcreteState [].

  Definition concreteStep (st : concreteState) (obs : option observation)
    (st0 : concreteState) :=
    step (devices st) obs (devices st0).

  Inductive abstractStep : abst_state -> option observation -> abst_state -> 
    Prop := 
  | AbstractStep : forall sw pt pk lps,
    abstractStep
      ({| (sw,pt,pk) |} <+> lps)
      (Some (sw,pt,pk))
      (unions (map (transfer sw) (abst_func sw pt pk)) <+> lps).

  Definition relate_switch (sw : switch) : abst_state :=
    match sw with
      | Switch swId _ tbl inp outp ctrlm switchm =>
        from_list (map (affixSwitch swId) (to_list inp)) <+>
        unions (map (transfer swId) (to_list outp)) <+>
        unions (map (select_packet_out swId) (to_list ctrlm)) <+>
        unions (map (select_packet_in swId) (to_list switchm))
    end.

  Definition relate_dataLink (link : dataLink) : abst_state :=
    match link with
      | DataLink _ pks (sw,pt) =>
        from_list (map (fun pk => (sw,pt,pk)) pks)
    end.

  Definition relate_openFlowLink (link : openFlowLink) : abst_state :=
    match link with
      | OpenFlowLink sw switchm ctrlm =>
        unions (map (select_packet_out sw) ctrlm) <+>
        unions (map (select_packet_in sw) switchm)
    end.

  Definition relate (st : state) : abst_state :=
    unions (map relate_switch (to_list (switches st))) <+>
    unions (map relate_dataLink (links st)) <+>
    unions (map relate_openFlowLink (ofLinks st)) <+>
    relate_controller (ctrl st).

  Definition bisim_relation : relation concreteState abst_state :=
    fun (st : concreteState) (ast : abst_state) => 
      ast = (relate (devices st)).

  Module AtomsAndController := AtomsAndController.

End Make.

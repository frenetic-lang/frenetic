Set Implicit Arguments.

Require Import Coq.Lists.List.
Require Import Coq.Classes.Equivalence.
Require Import Coq.Structures.Equalities.
Require Import Coq.Classes.Morphisms.
Require Import Coq.Setoids.Setoid.
Require Import Common.Types.
Require Import Common.Bisimulation.
Require Import Common.AllDiff.
Require Import Bag.Bag.
Require Import FwOF.FwOF.

Local Open Scope list_scope.
Local Open Scope equiv_scope.
Local Open Scope bag_scope.

Module Make (AtomsAndController : ATOMS_AND_CONTROLLER).
  Export AtomsAndController.

    (** "FMS" is short for "flow mod safety". *)
    Inductive FMS : switch -> openFlowLink -> Prop := 
    | NoFlowModsInBuffer : forall swId pts tbl inp outp ctrlm switchm
                                  ctrlmList switchmList,
      (forall msg, Mem msg ctrlm -> NotFlowMod msg) ->
      (exists ctrlEp, SafeWire swId ctrlEp ctrlmList (Endpoint_Barrier tbl)) ->
      FMS (Switch swId pts tbl inp outp ctrlm switchm)
          (OpenFlowLink swId switchmList ctrlmList)
    | OneFlowModInBuffer : forall swId pts tbl inp outp ctrlm ctrlm0 switchm 
                                  ctrlmList switchmList f,
      (forall msg, Mem msg ctrlm0 -> NotFlowMod msg) ->
      (exists ctrlEp, SafeWire swId ctrlEp ctrlmList 
                               (Endpoint_NoBarrier (modify_flow_table f tbl))) ->

      ctrlm === ({|FlowMod f|} <+> ctrlm0) ->
      FlowTableSafe swId (modify_flow_table f tbl) ->
      FMS (Switch swId pts tbl inp outp ctrlm switchm)
          (OpenFlowLink swId switchmList ctrlmList).

    Definition AllFMS (sws : bag switch) (ofLinks : list openFlowLink) :=
      forall sw,
        Mem sw sws ->
        exists lnk, 
          In lnk ofLinks /\
          of_to lnk = swId sw /\
          FMS sw lnk.
    
  Definition affixSwitch (sw : switchId) (ptpk : portId * packet) :=
    match ptpk with
      | (pt,pk) => (sw,pt,pk)
    end.

  Definition FlowTablesSafe (sws : bag switch) : Prop :=
    forall swId pts tbl inp outp ctrlm switchm,
      Mem (Switch swId pts tbl inp outp ctrlm switchm) sws ->
      FlowTableSafe swId tbl.

  Definition ConsistentDataLinks (links : list dataLink) : Prop :=
    forall (lnk : dataLink),
      In lnk links ->
      topo (src lnk) = Some (dst lnk).

  Definition LinkHasSrc (sws : bag switch) (link : dataLink) : Prop :=
    exists switch,
      Mem switch sws /\
      fst (src link) = swId switch /\
      In (snd (src link)) (pts switch).

  Definition LinkHasDst (sws : bag switch) (link : dataLink) : Prop :=
    exists switch,
      Mem switch sws /\
      fst (dst link) = swId switch /\
      In (snd (dst link)) (pts switch).

  Definition LinksHaveSrc (sws : bag switch) (links : list dataLink) :=
    forall link, In link links -> LinkHasSrc sws link.

  Definition LinksHaveDst (sws : bag switch) (links : list dataLink) :=
    forall link, In link links -> LinkHasDst sws link.

  Definition UniqSwIds (sws : bag switch) := AllDiff swId (Bag.to_list sws).

  Record concreteState := ConcreteState {
    devices : state;
    concreteState_flowTableSafety : FlowTablesSafe (switches devices);
    concreteState_consistentDataLinks : ConsistentDataLinks (links devices);
    linksHaveSrc : LinksHaveSrc (switches devices) (links devices);
    linksHaveDst : LinksHaveDst (switches devices) (links devices);
    uniqSwIds : UniqSwIds (switches devices);
    allFMS : AllFMS (switches devices) (ofLinks devices)
  }.

  Implicit Arguments ConcreteState [].

  Definition concreteStep (st : concreteState) (obs : option observation)
    (st0 : concreteState) :=
    step (devices st) obs (devices st0).

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
        FromList (map (affixSwitch swId) (Bag.to_list inp)) <+>
        Bag.unions (map (transfer swId) (Bag.to_list outp)) <+>
        Bag.unions (map (select_packet_out swId) (Bag.to_list ctrlm)) <+>
        Bag.unions (map (select_packet_in swId) (Bag.to_list switchm))
    end.

  Definition relate_dataLink (link : dataLink) : abst_state :=
    match link with
      | DataLink _ pks (sw,pt) =>
        FromList (map (fun pk => (sw,pt,pk)) pks)
    end.

  Definition relate_openFlowLink (link : openFlowLink) : abst_state :=
    match link with
      | OpenFlowLink sw switchm ctrlm =>
        Bag.unions (map (select_packet_out sw) ctrlm) <+>
        Bag.unions (map (select_packet_in sw) switchm)
    end.


  Definition relate (st : state) : abst_state :=
    Bag.unions (map relate_switch (Bag.to_list (switches st))) <+>
    Bag.unions (map relate_dataLink (links st)) <+>
    Bag.unions (map relate_openFlowLink (ofLinks st)) <+>
    relate_controller (ctrl st).

  Definition bisim_relation : relation concreteState abst_state :=
    fun (st : concreteState) (ast : abst_state) => 
      ast === (relate (devices st)).

End Make.


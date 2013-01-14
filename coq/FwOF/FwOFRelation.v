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

Module Make (Import Atoms : ATOMS) (MakeControllerLemmas : CONTROLLER_LEMMAS).

  Module Export ControllerLemmas := MakeControllerLemmas (Atoms).

  Definition affixSwitch (sw : switchId) (ptpk : portId * packet) :=
    match ptpk with
      | (pt,pk) => (sw,pt,pk)
    end.

  Definition FlowTableSafe (sw : switchId) (tbl : flowTable) : Prop :=
    forall pt pk forwardedPkts packetIns,
      process_packet tbl pt pk = (forwardedPkts, packetIns) ->
      Bag.unions (map (transfer sw) forwardedPkts) <+>
      Bag.unions (map (select_packet_in sw) (map (PacketIn pt) packetIns)) ===
      Bag.unions (map (transfer sw) (abst_func sw pt pk)).

  Definition FlowTablesSafe (sws : bag switch) : Prop :=
    forall swId pts tbl inp outp ctrlm switchm,
      Mem (Switch swId pts tbl inp outp ctrlm switchm) sws ->
      FlowTableSafe swId tbl.

  Definition ConsistentDataLinks (links : list dataLink) : Prop :=
    forall (lnk : dataLink),
      In lnk links ->
      topo (src lnk) = Some (dst lnk).

  (* Slightly annoying since it is defined over the entire system state.
     Stronger than needed, because it holds the other devices static.
     But, this is easy to weaken! Use simpl_multistep lemma! *)
  Axiom ControllerLiveness : forall sw pt pk ctrl0 sws0 links0 ofLinks0,
    Mem (sw,pt,pk) (relate_controller ctrl0) ->
    exists  ofLinks10 ofLinks11 ctrl1 swTo ptTo switchmLst ctrlmLst,
      (multistep 
         step (State sws0 links0 ofLinks0 ctrl0) nil
         (State sws0 links0
                (ofLinks10 ++ 
                 (OpenFlowLink swTo switchmLst 
                  (PacketOut ptTo pk :: ctrlmLst)) ::
                 ofLinks11) 
                ctrl1)) /\
      select_packet_out swTo (PacketOut ptTo pk) = ({|(sw,pt,pk)|}).


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

  Section FlowModSafety.

    Inductive Endpoint : Type :=
    | Endpoint_NoBarrier : flowTable -> Endpoint
    | Endpoint_Barrier : flowTable -> Endpoint.

    Definition table_at_endpoint (ep : Endpoint) :=
      match ep with
        | Endpoint_NoBarrier tbl => tbl
        | Endpoint_Barrier tbl => tbl
      end.

    Inductive SafeWire : switchId -> 
                    Endpoint -> 
                    list fromController ->
                    Endpoint -> Prop :=
    | SafeWire_nil : forall swId tbl,
      FlowTableSafe swId tbl ->
      SafeWire swId (Endpoint_NoBarrier tbl) nil (Endpoint_NoBarrier tbl)
    | SafeWire_PktOut : forall swId pt pk ctrlEp ctrlm swEp,
      SafeWire swId ctrlEp ctrlm swEp ->
      SafeWire swId ctrlEp (PacketOut pt pk :: ctrlm) swEp
    | SafeWire_BarrierRequest : forall swId n ctrlEp ctrlm swEp,
      SafeWire swId ctrlEp ctrlm swEp ->
      SafeWire swId (Endpoint_Barrier (table_at_endpoint ctrlEp))
                    (BarrierRequest n :: ctrlm)
                    swEp
    | SafeWire_FlowMod : forall swId f tbl ctrlm swEp,
      FlowTableSafe swId (modify_flow_table f tbl) ->
      SafeWire swId (Endpoint_Barrier tbl) ctrlm swEp ->
      SafeWire swId (Endpoint_NoBarrier (modify_flow_table f tbl))
          (FlowMod f :: ctrlm) swEp.

    Inductive NotFlowMod : fromController -> Prop :=
    | NotFlowMod_BarrierRequest : forall n, NotFlowMod (BarrierRequest n)
    | NotFlowMod_PacketOut : forall pt pk, NotFlowMod (PacketOut pt pk).

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
    
    End FlowModSafety.

    
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


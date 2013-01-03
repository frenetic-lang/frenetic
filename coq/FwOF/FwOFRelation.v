Set Implicit Arguments.

Require Import Coq.Lists.List.
Require Import Coq.Classes.Equivalence.
Require Import Coq.Structures.Equalities.
Require Import Common.Types.
Require Import Bag.Bag.
Require Import FwOF.FwOF.

Local Open Scope list_scope.
Local Open Scope equiv_scope.
Local Open Scope bag_scope.


Module Make (Import Atoms : ATOMS).

  Module Concrete := ConcreteSemantics (Atoms).
  Import Concrete.

  Definition abst_state := Bag.Bag (switchId * portId * packet).
  Axiom abst_func : switchId -> portId -> packet -> list (portId * packet).
  Axiom topo : switchId -> portId -> option (switchId * portId).

  Definition affixSwitch (sw : switchId) (ptpk : portId * packet) :=
    match ptpk with
      | (pt,pk) => (sw,pt,pk)
    end.
  
  Inductive abstractStep : abst_state -> option observation -> abst_state -> 
    Prop := 
  | AbstractStep : forall sw pt pk lps,
    abstractStep
      ({| (sw,pt,pk) |} <+> lps)
      (Some (sw,pt,pk))
      (Bag.from_list (map (affixSwitch sw) (abst_func sw pt pk)) <+> lps).

  Definition transfer (sw : switchId) (ptpk : portId * packet) :=
    match ptpk with
      | (pt,pk) =>
        match topo sw pt with
          | Some (sw',pt') => {| (sw',pt',pk) |}
          | None => {| |}
        end
    end.

  Definition select_packet_out (sw : switchId) (msg : fromController) :=
    match msg with
      | PacketOut pt pk => {| (sw,pt,pk) |}
      | _ => {| |}
    end.

  Axiom locate_packet_in : switchId -> portId -> packet -> 
    list (portId * packet).

  Definition select_packet_in (sw : switchId) (msg : fromSwitch) :=
    match msg with
      | PacketIn pt pk => 
        Bag.from_list (map (affixSwitch sw) (locate_packet_in sw pt pk))
      | _ => {| |}
    end.

  Definition relate_switch (sw : switch) : abst_state :=
    match sw with
      | Switch swId _ tbl inp outp ctrlm switchm =>
        Bag.map (affixSwitch swId) inp <+>
        bag_unions (map (transfer swId) (Bag.to_list outp)) <+>
        bag_unions (map (select_packet_out swId) (Bag.to_list ctrlm)) <+>
        bag_unions (map (select_packet_in swId) (Bag.to_list switchm))
    end.

  Definition relate_dataLink (link : dataLink) : abst_state :=
    match link with
      | DataLink _ pks (sw,pt) =>
        Bag.from_list (map (fun pk => (sw,pt,pk)) pks)
    end.

  Definition relate_openFlowLink (link : openFlowLink) : abst_state :=
    match link with
      | OpenFlowLink sw switchm ctrlm =>
        bag_unions (map (select_packet_out sw) ctrlm) <+>
        bag_unions (map (select_packet_in sw) switchm)
    end.

  Axiom relate_controller : controller -> abst_state.

  Definition relate (st : state) : abst_state :=
    match st with
      | (switches, links, ofLinks, ctrl) =>
        bag_unions (map relate_switch switches) <+>
        bag_unions (map relate_dataLink links) <+>
        bag_unions (map relate_openFlowLink ofLinks) <+>
        relate_controller ctrl
    end.
        
  Definition bisim_relation (st : state) (ast : abst_state) : Prop :=
    ast === (relate st).


End Make.    
  

  

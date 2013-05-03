Set Implicit Arguments.

Require Import Coq.Lists.List.
Require Import Coq.Bool.Bool.
Require Import Common.Types.
Require Import Word.WordInterface.
Require Import Network.NetworkPacket.
Require Import Pattern.Pattern.
Require Import OpenFlow.OpenFlow0x01Types.
Require Import NetCore.NetCoreAction.

Import ListNotations.
Local Open Scope list_scope.
Local Open Scope bool_scope.
  
Inductive pred : Type := 
| PrHdr : pattern ->  pred
| PrOnSwitch : switchId -> pred
| PrOr : pred -> pred -> pred
| PrAnd : pred -> pred -> pred
| PrNot : pred -> pred
| PrAll : pred
| PrNone : pred.

Inductive pol : Type :=
| PoAction : NetCoreAction.t -> pol
| PoFilter : pred -> pol
| PoUnion : pol -> pol -> pol
| PoSeq : pol -> pol -> pol.

Inductive value : Type :=
| ValPkt : switchId -> portId -> packet -> bufferId + bytes -> value
| ValGetPkt : NetCoreAction.id -> switchId -> portId -> packet -> value
| ValNothing : value.

Fixpoint match_pred (pr : pred) (sw : switchId) (pt : portId) (pk : packet) := 
  match pr with
    | PrHdr pat => Pattern.match_packet pt pk pat
    | PrOnSwitch sw' =>
      match Word64.eq_dec sw sw' with
        | left _ => true
        | right _ => false
      end
    | PrOr p1 p2 => match_pred p1 sw pt pk || match_pred p2 sw pt pk
    | PrAnd p1 p2 => match_pred p1 sw pt pk && match_pred p2 sw pt pk
    | PrNot p' => negb (match_pred p' sw pt pk)
    | PrAll => true
    | PrNone => false
  end.

Parameter serialize_pkt : packet -> bytes.

Extract Constant serialize_pkt => "Packet_Parser.serialize_packet".

Definition eval_action (inp : value) (act : NetCoreAction.t) : list value :=
  match inp with
    | ValPkt sw pt pk buf => 
      map 
        (fun ptpk =>
           match ptpk with | (pt', pk') => ValPkt sw pt' pk' buf end)
        (NetCoreAction.apply_action act pt pk) ++
        map (fun qid => ValGetPkt qid sw pt pk) (NetCoreAction.queries act)
    | _ => [inp]
  end.

Fixpoint classify (p : pol) (inp : value) := 
  match p with
    | PoAction action => eval_action inp action
    | PoFilter pred =>
      match inp with
        | ValPkt sw pt pk buf => if match_pred pred sw pt pk then [inp] else nil
        | _ => [inp]
      end
    | PoUnion p1 p2 => classify p1 inp ++ classify p2 inp
    | PoSeq p1 p2 => concat_map (classify p2) (classify p1 inp)
  end.

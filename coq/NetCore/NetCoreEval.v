Set Implicit Arguments.

Require Import Coq.Lists.List.
Require Import Coq.Bool.Bool.

Require Import Common.Types.
Require Import Word.WordInterface.
Require Import Classifier.Classifier.
Require Import Network.NetworkPacket.
Require Import Pattern.Pattern.
Require Import OpenFlow.OpenFlow0x01Types.
Require Import NetCore.NetCoreAction.

Local Open Scope list_scope.
  
Inductive pred : Type := 
| PrHdr : pattern ->  pred
| PrOnSwitch : switchId -> pred
| PrOr : pred -> pred -> pred
| PrAnd : pred -> pred -> pred
| PrNot : pred -> pred
| PrAll : pred
| PrNone : pred.

Inductive pol : Type :=
| PoAtom : pred -> NetCoreAction.t -> pol
| PoUnion : pol -> pol -> pol
| PoSeq : pol -> pol -> pol.

Inductive input : Type :=
| InPkt : switchId -> portId -> packet -> option bufferId -> input.

Inductive output : Type :=
| OutPkt : switchId -> pseudoPort -> packet -> bufferId + bytes -> output
| OutGetPkt : NetCoreAction.id -> switchId -> portId -> packet -> output
| OutNothing : output.

Definition is_OutPkt outp := 
  match outp with
    | OutPkt _ _ _ _ => true
    | _ => false
  end.

Fixpoint match_pred (pr : pred) (sw : switchId) (pt : portId) (pk : packet) := 
  match pr with
    | PrHdr pat => Pattern.match_packet pt pk pat
    | PrOnSwitch sw' => match Word64.eq_dec sw sw' with
                          | left _ => true
                          | right _ => false
                        end
    | PrOr p1 p2 => orb (match_pred p1 sw pt pk) (match_pred p2 sw pt pk)
    | PrAnd p1 p2 => andb (match_pred p1 sw pt pk) (match_pred p2 sw pt pk)
    | PrNot p' => negb (match_pred p' sw pt pk)
    | PrAll => true
    | PrNone => false
  end.

Parameter serialize_pkt : packet -> bytes.

Extract Constant serialize_pkt => "Packet_Parser.serialize_packet".

Definition outp_to_inp outp :=
  match outp with
    (* TODO(arjun): this looks bogus. When the first policy floods, what port
       does the input packet(s) to the 2nd policy have? *)
    | OutPkt sw (PhysicalPort pt) pk (inl bufId) => Some (InPkt sw pt pk (Some bufId))
    | _ => None
  end.

Definition eval_action (inp : input) (act : NetCoreAction.t) : list output := 
  match inp with
    | InPkt sw pt pk buf => 
      map (fun ptpk =>
             match ptpk with
               | (pt', pk') => 
                 OutPkt sw (PhysicalPort pt') pk' 
                        (match buf with
                           | Some bufId => inl bufId
                           | None => inr (serialize_pkt pk')
                         end)
             end)
          (NetCoreAction.apply_action act pt pk) ++
      map (fun qid => OutGetPkt qid sw pt pk) (NetCoreAction.queries act)
  end.

Fixpoint classify (p : pol) (inp : input) := 
  match p with
    | PoAtom pr actions => 
      match inp with
        | InPkt sw pt pk buf => 
          eval_action inp
            (if match_pred pr sw pt pk then actions else NetCoreAction.zero)
      end
    | PoUnion p1 p2 => 
      classify p1 inp ++ classify p2 inp
    | PoSeq p1 p2 =>
      let (outPkts1, queries1) := List.partition is_OutPkt (classify p1 inp) in
      queries1 ++ (concat_map (classify p2) (filter_map outp_to_inp outPkts1))
  end.

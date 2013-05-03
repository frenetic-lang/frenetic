Set Implicit Arguments.

Require Import Coq.Lists.List.
Require Import Classifier.ClassifierAction.
Require Import Classifier.Classifier.
Require Import Common.Types.
Require Import Word.WordInterface.
Require Import Pattern.Pattern.
Require Import Network.NetworkPacket.
Require Import OpenFlow.OpenFlow0x01Types.

Import ListNotations.
Local Open Scope list_scope.

Module Type NETCORE_ACTION.

  Inductive id : Type := MkId : nat -> id.

  Include ACTION_DEF.

  (* An action that forwards out of a given port *)
  Parameter forward : portId -> t.

  (* [updateDlSrc oldDlSrc newDlSrc] updates the source MAC address of
     a packet to [newDlSrc], only if its current value is [oldDlSrc]. *)
  Parameter updateDlSrc : dlAddr -> dlAddr -> t.

  Parameter apply_action : t -> portId -> packet -> list (portId * packet).
  Parameter queries : t -> list id.

  (** Returns an OpenFlow 1.0 action sequence that corresponds to this
      NetCore action. This action sequence can then be used in a flow table.

      The input-port argument must be the input port matched by the
      associated rule in the flow table. This is needed to use the IN_PORT 
      action correctly.

      If the NetCore action has any queries, the action sequence will include
      one CONTROLLER action, sending upto 65K of the packet to the controller.
      This action is the last action in the sequence (the reference user-switch
      requires it to be last). *)
  Parameter as_actionSequence : option portId -> t -> actionSequence.

End NETCORE_ACTION.

Module NetCoreAction : NETCORE_ACTION.

  Inductive id : Type := MkId : nat -> id.

  Definition match_modify (A : Type) := option (A * A).

  Record output : Type := 
    Output {
        outDlSrc : match_modify dlAddr;
        outDlDst : match_modify dlAddr;
        outDlVlan : match_modify (option dlVlan);
        outDlVlanPcp : match_modify dlVlanPcp;
        outNwSrc : match_modify nwAddr;
        outNwDst : match_modify nwAddr;
        outNwTos : match_modify nwTos;
        outTpSrc : match_modify tpPort;
        outTpDst : match_modify tpPort;
        outPort : option portId (* TODO(arjun): refactor later to support flood *)
      }.

  Record act : Type := 
    Act {
        outputs : list output;
        queries : list id
      }.

  Definition zero := Act nil nil.

  Definition one : act :=
    Act [Output None None None None None None None None None None]
        nil.

  Definition forward (pt : portId) :=
    Act [Output None None None None None None None None None (Some pt)]
        nil.

  Definition updateDlSrc (old new : dlAddr) :=
    Act [Output (Some (old, new)) None None None None None None None None None]
        nil.

  Definition par_action (act1 act2 : act) : act :=
    match (act1, act2) with
      | (Act outs1 q1, Act outs2 q2) => Act (outs1 ++ outs2) (q1 ++ q2)
    end.

  Definition seq_mod {A : Type} (m1 m2 : match_modify A) :=
    match (m1, m2) with
      | (None, _) => m2
      | (Some (old_value, _), Some (_, new_value)) => Some (old_value, new_value)
      (* TODO(arjun): how is this ok??? *)
      | (Some (x, y), None) => m1
    end.

  Definition seq_port (pt1 pt2 : option portId) :=
    match (pt1, pt2) with
      | (_, Some pt) => Some pt
      | (Some pt, None) => Some pt
      | (None, None) => None
    end.

  Definition seq_output (out1 out2 : output) :=
    match (out1, out2) with
      | (Output dlSrc1 dlDst1 dlVlan1 dlVlanPcp1 
                nwSrc1 nwDst1 nwTos1 tpSrc1 tpDst1 pt1,
         Output dlSrc2 dlDst2 dlVlan2 dlVlanPcp2
                nwSrc2 nwDst2 nwTos2 tpSrc2 tpDst2 pt2) =>
        Output (seq_mod dlSrc1 dlSrc2)
               (seq_mod dlDst1 dlDst2)
               (seq_mod dlVlan1 dlVlan2)
               (seq_mod dlVlanPcp1 dlVlanPcp2)
               (seq_mod nwSrc1 nwSrc2)
               (seq_mod nwDst1 nwDst2)
               (seq_mod nwTos1 nwTos2)
               (seq_mod tpSrc1 tpSrc2)
               (seq_mod tpDst1 tpDst2)
               (seq_port pt1 pt2)
    end.

  Definition seq_action (act1 act2 : act) : act :=
    match (act1, act2) with
      | (Act outs1 q1, Act outs2 q2) => 
        Act (outs1 ++ outs2) (q1 ++ q2)
    end.
  
  Section MaskPat.

    Definition trans {A : Type} 
               (x : match_modify A) (f : A -> pattern -> pattern)
               (pat : pattern) :=
      match x with
        | None => pat
        | Some (_, new) => f new pat
      end.
    
    Local Notation "f $ x" := (f x) (at level 51, right associativity).

    (** [mask_pat atom pat] transforms [pat] to account for how [atom]
         modifies packets. If [pat] matches an input packet [(pt,pk)],
         then the transformed pattern matches the output [apply_atom
         atom (pt,pk)], and vice versa.

         Therefore, in a flow table, if [pat] is the pattern in a rule
         and [atom] is an action, [mask_pat] can be used to match the
         packets that [atom] produces. *)
    Definition mask_pat (out : output) (pat : pattern) : pattern :=
      match out with
        | (Output dlSrc dlDst dlVlan dlVlanPcp nwSrc nwDst nwTos tpSrc tpDst
                  outPort) =>
           trans dlSrc Pattern.setDlSrc $
           trans dlDst Pattern.setDlDst pat
      end.

  End MaskPat.

  Section ApplyAtom.

    Definition maybe_modify {A : Type} (newVal : match_modify A) 
               (modifier : packet -> A -> packet) (pk : packet) : packet :=
      match newVal with
        | None => pk
        | Some (_, v) => modifier pk v
      end.
    
    Definition withVlanNone (maybeVlan : match_modify (option dlVlan)) :=
      match maybeVlan with
        | None => None
        | Some (None, None) => Some (VLAN_NONE, VLAN_NONE)
        | Some (Some old, None) => Some (old, VLAN_NONE)
        | Some (None, Some new) => Some (VLAN_NONE, new)
        | Some (Some old, Some new) => Some (old, new)
      end.

    (* Unlike Haskell, $ is not a function. *)
    Local Notation "f $ x" := (f x) (at level 51, right associativity).

    Definition apply_atom (out : output) (ptpk : portId * packet) :=
      match (out, ptpk) with
        | (Output dlSrc dlDst dlVlan dlVlanPcp nwSrc nwDst nwTos 
                  tpSrc tpDst (Some outPort),
           (_, pk)) =>
          (outPort,
           maybe_modify dlSrc setDlSrc $
           maybe_modify dlDst setDlDst $
           maybe_modify (withVlanNone dlVlan) setDlVlan $
           maybe_modify dlVlanPcp setDlVlanPcp $
           maybe_modify nwSrc setNwSrc $
           maybe_modify nwDst setNwDst $
           maybe_modify nwTos setNwTos $
           maybe_modify tpSrc setTpSrc $
           maybe_modify tpDst setTpDst pk)
        | (_, (pt, pk)) => (pt, pk)
      end.

  End ApplyAtom.

  Definition apply_action action pt pk :=
    List.map (fun out => apply_atom out (pt,pk)) (outputs action).

  Section Compile.

    Definition sel {A : Type} (f : A -> pattern) (x : match_modify A) :=
      match x with
        | None => Pattern.all
        | Some (old, _) => f old
      end.

    (* TODO(arjun): restrict other fields. Need to be fancy for TCP/IP fields. *)
    Definition compile_out (out : output) :=
      match out with
        | (Output dlSrc dlDst dlVlan dlVlanPcp 
                  nwSrc nwDst nwTos tpSrc tpDst pt) =>
          [(List.fold_right 
              Pattern.inter 
              Pattern.all
              [sel Pattern.dlSrc dlSrc; 
                sel Pattern.dlDst dlDst],
            Act [out] nil)]
      end.

    Definition compile (action : act) :=
      match action with
        | Act outs nil =>
          unions par_action (map compile_out outs)
        | Act outs qs =>
          union par_action [(Pattern.all, Act nil qs)]
                (unions par_action (map compile_out outs))
      end.

  End Compile.

  Section OpenFlow0x01.

    Definition set {A : Type} (upd : match_modify A) (mk : A -> action)
               (lst : actionSequence) :=
      match upd with
        | Some (_, new) => (mk new) :: lst
        | None => lst
      end.

    Definition unset {A : Type} (upd : match_modify A) (mk : A -> action)
               (lst : actionSequence) :=
      match upd with
        | Some (old, _) => (mk old) :: lst
        | None => lst
      end.

    Definition setDlVlan' (vlan : option dlVlan) := 
      match vlan with
        | None => StripVlan
        | Some n => SetDlVlan n
      end.

    Local Notation "f $ x" := (f x) (at level 51, right associativity).

    Definition modify (out : output) : actionSequence := 
      match out with
        | (Output dlSrc dlDst dlVlan dlVlanPcp nwSrc nwDst nwTos 
                  tpSrc tpDst outPort) =>
          set dlSrc SetDlSrc $
          set dlDst SetDlDst $
          set dlVlan setDlVlan' $
          set dlVlanPcp SetDlVlanPcp $
          set nwSrc SetNwSrc $
          set nwDst SetNwDst $
          set nwTos SetNwTos $
          set tpSrc SetTpSrc $
          set tpDst SetTpDst $
          nil
      end.

    (* It is tempting to write a function that takes set and unset as
       a parameter. But, that's rank-2 polymorphism, which extracts to
       very fishy code. *)
    Definition unmodify (out : output) : actionSequence := 
      match out with
        | (Output dlSrc dlDst dlVlan dlVlanPcp nwSrc nwDst nwTos 
                  tpSrc tpDst outPort) =>
          unset dlSrc SetDlSrc $
          unset dlDst SetDlDst $
          unset dlVlan setDlVlan' $
          unset dlVlanPcp SetDlVlanPcp $
          unset nwSrc SetNwSrc $
          unset nwDst SetNwDst $
          unset nwTos SetNwTos $
          unset tpSrc SetTpSrc $
          unset tpDst SetTpDst $
          nil
      end.

    Definition output_to_of (inp : option portId) (out : output) : 
      actionSequence :=
      match outPort out with
        | None => []
        | Some pt =>
          modify out ++
          match inp with
            | None => OpenFlow0x01Types.Output (PhysicalPort pt)
            | Some pt' =>
              match Word16.eq_dec pt' pt with
                | left _ => OpenFlow0x01Types.Output InPort
                | right _ => OpenFlow0x01Types.Output (PhysicalPort pt)
              end
          end ::
          unmodify out
      end.

    Definition as_actionSequence (inp : option portId) 
               (action : act) : actionSequence :=
      match action with
        | Act outs nil => concat_map (output_to_of inp) outs
        | Act outs (_ :: _) => 
          concat_map (output_to_of inp) outs ++ 
          [OpenFlow0x01Types.Output (Controller Word16.max_value)]
      end.

  End OpenFlow0x01.          

  Definition t := act.
  Definition e := output.


  Definition is_emit_output out :=
    match outPort out with
      | None => false
      | Some _ => true
    end.

  Definition atoms action := 
    filter is_emit_output (outputs action).

End NetCoreAction.

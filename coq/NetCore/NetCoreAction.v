Set Implicit Arguments.

Require Import Coq.Lists.List.
Require Import Classifier.ClassifierSignatures.
Require Import Common.Types.
Require Import Word.WordInterface.
Require Import Pattern2.PatternSignatures.
Require Pattern2.PatternImpl.
Require Pattern2.PatternTheory.
Require Import Network.NetworkPacket.
Require Import OpenFlow.OpenFlow0x01Types.

Import ListNotations.
Local Open Scope list_scope.

Module Port <: PORT.

  Inductive port : Type :=
    | Physical : portId -> port
    | Here : port
    | Bucket : nat-> port.
  
  Lemma eqdec : forall (x y : port), { x = y } + { x <> y }.
  Proof.
    decide equality. apply Word16.eq_dec. apply eqdec.
  Qed.

  Definition opt_portId x :=
    match x with
      | Physical pt => Some pt
      | Here => None
      | Bucket _ => None
    end.

  Definition t := port.

End Port.

Module Type NETCORE_ACTION.

  Include ACTION.

  (* An action that forwards out of a given port *)
  Parameter forward : portId -> t.

  (* An action that sends the packet to the indicated bucket *)
  Parameter bucket : nat -> t.

  (* [updateDlSrc oldDlSrc newDlSrc] updates the source MAC address of
     a packet to [newDlSrc], only if its current value is [oldDlSrc]. *)
  Parameter updateDlSrc : dlAddr -> dlAddr -> t.

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

Module NetCoreAction <: NETCORE_ACTION.

  Module PatternSpec := Pattern2.PatternTheory.Make (Port).
  Module Pattern := PatternSpec.Pattern.

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
        outPort : Pattern.port
      }.

  Definition act := list output.

  Definition drop : act := nil.

  Definition pass := [Output None None None None None None None None None Port.Here].

  Definition forward (pt : portId) :=
    [Output None None None None None None None None None (Port.Physical pt)].

  Definition bucket (n : nat) :=
    [Output None None None None None None None None None (Port.Bucket n)].

  Definition updateDlSrc (old new : dlAddr) :=
    [Output (Some (old, new)) None None None None None None None None Port.Here].

  Definition par_action (act1 act2 : act) : act := act1 ++ act2.

  Definition seq_mod {A : Type} (beq : A -> A -> bool) (m1 m2 : match_modify A) :=
    match (m1, m2) with
      | (None, _) => Some m2
      | (Some (v1, v2), Some (v3, v4)) => 
         match beq v2 v3 with
           | true => Some (Some (v1, v4))
           | false => None
         end
      | (Some (x, y), None) => Some m1
    end.

  Definition seq_port (pt1 pt2 : Pattern.port) := 
    match (pt1, pt2) with
      | (Port.Here, _) => pt2
      | (_, Port.Here) => pt1
      | _ => pt2
    end.

  Definition optword16beq w1 w2 :=
    match (w1, w2) with
      | (None, None) => true
      | (Some w1, Some w2) => Word16.beqdec w1 w2
      | _ => false
    end.

  Definition seq_output (out1 out2 : output) :=
    match (out1, out2) with
      | (Output dlSrc1 dlDst1 dlVlan1 dlVlanPcp1 
                nwSrc1 nwDst1 nwTos1 tpSrc1 tpDst1 pt1,
         Output dlSrc2 dlDst2 dlVlan2 dlVlanPcp2
                nwSrc2 nwDst2 nwTos2 tpSrc2 tpDst2 pt2) =>
        match (seq_mod Word48.beqdec dlSrc1 dlSrc2,
               seq_mod Word48.beqdec dlDst1 dlDst2,
               seq_mod optword16beq dlVlan1 dlVlan2,
               seq_mod Word8.beqdec dlVlanPcp1 dlVlanPcp2,
               seq_mod Word32.beqdec nwSrc1 nwSrc2,
               seq_mod Word32.beqdec nwDst1 nwDst2,
               seq_mod Word8.beqdec nwTos1 nwTos2,
               seq_mod Word16.beqdec tpSrc1 tpSrc2,
               seq_mod Word16.beqdec tpDst1 tpDst2) with
          | (Some dlSrc,
             Some dlDst,
             Some dlVlan,
             Some dlVlanPcp,
             Some nwSrc,
             Some nwDst,
             Some nwTos,
             Some tpSrc,
             Some tpDst) =>
            Some (Output dlSrc dlDst dlVlan dlVlanPcp nwSrc nwDst nwTos tpSrc tpDst
                         (seq_port pt1 pt2))
          | _ => None
        end
    end.

  Definition cross {A B : Type} (lst1 : list A) (lst2 : list B) : list (A * B) :=
    concat_map
      (fun a => map (fun b => (a, b)) lst2)
      lst1.

  Definition seq_action (act1 act2 : act) : act :=
    filter_map 
      (fun (o1o2 : output * output) => let (o1,o2) := o1o2 in seq_output o1 o2)
      (cross act1 act2).
  
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

    (** TODO(arjun): this is wrong, IMO. *)
    Definition apply_atom (out : output) (ptpk : Pattern.port * packet) :=
      match (out, ptpk) with
        | (Output dlSrc dlDst dlVlan dlVlanPcp nwSrc nwDst nwTos 
                  tpSrc tpDst outPort,
           (_, pk)) =>
          Some (outPort,
           maybe_modify dlSrc setDlSrc $
           maybe_modify dlDst setDlDst $
           maybe_modify (withVlanNone dlVlan) setDlVlan $
           maybe_modify dlVlanPcp setDlVlanPcp $
           maybe_modify nwSrc setNwSrc $
           maybe_modify nwDst setNwDst $
           maybe_modify nwTos setNwTos $
           maybe_modify tpSrc setTpSrc $
           maybe_modify tpDst setTpDst pk)
      end.

  End ApplyAtom.

  Section Compile.

    Definition trans {A : Type} 
               (x : match_modify A) (f : A -> Pattern.t -> Pattern.t)
               (pat : Pattern.t) :=
      match x with
        | None => pat
        | Some (_, new) => f new pat
      end.

    Definition sel {A : Type} (f : A -> Pattern.t) (x : match_modify A) :=
      match x with
        | None => Pattern.all
        | Some (old, _) => f old
      end.

    Local Notation "f $ x" := (f x) (at level 51, right associativity).

    Definition restrict_range (out : output) pat :=
      match out with
        | (Output dlSrc dlDst dlVlan dlVlanPcp nwSrc nwDst nwTos tpSrc tpDst
                  outPort) =>
           trans dlSrc Pattern.setDlSrc $
           trans dlDst Pattern.setDlDst pat
      end.

    Definition domain (out : output) :=
      match out with
        | (Output dlSrc dlDst dlVlan dlVlanPcp 
                  nwSrc nwDst nwTos tpSrc tpDst pt) =>
          List.fold_right 
            Pattern.inter 
            Pattern.all
            [sel Pattern.dlSrc dlSrc; 
              sel Pattern.dlDst dlDst]
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
        | Port.Physical pt =>
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
        | Port.Here => [] (* omg drop *)
        | Port.Bucket _ =>
          [OpenFlow0x01Types.Output (Controller Word16.max_value)]
      end.

    Definition as_actionSequence (inp : option portId) (action : act) :=
      concat_map (output_to_of inp) action.

  End OpenFlow0x01.          

  Definition t := act.
  Definition e := output.
  Definition pattern := Pattern.t.
  Definition port := Port.t.

  Definition atoms (action : t) : list e := action.

  Definition apply_action (action : t) (ptpk : Port.t * packet) :=
    filter_map (fun a => apply_atom a ptpk) action.

End NetCoreAction.

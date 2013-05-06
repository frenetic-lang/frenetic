Set Implicit Arguments.

Require Import Coq.Classes.Equivalence.
Require Import WordInterface.
Require Import Network.NetworkPacket.
Require Import OpenFlow.OpenFlow0x01Types.

Local Open Scope equiv_scope.

Module Type PORT.

  Parameter t : Type.
  Parameter eqdec : forall (x y : t), { x = y } + { x <> y }.
  Parameter opt_portId : t -> option portId.
  

End PORT.

Module Type PATTERN.

  Parameter port : Type.

  Parameter t : Type.

  Parameter inter : t -> t -> t.

  Parameter all : t.

  Parameter empty : t.

  Parameter exact_pattern : packet -> port -> t.

  Parameter is_empty : t -> bool.

  Parameter match_packet : port -> packet -> t -> bool.

  Parameter is_exact : t -> bool.

  (** Empty patterns and patterns that match on ports that are not switch ports
      cannot be translated to OpenFlow matches. *)
  Parameter to_match : t -> option of_match.

  Parameter beq : t -> t -> bool.

  (** Constructors that produce valid patterns. *)

  Parameter dlSrc : dlAddr -> t.

  Parameter dlDst : dlAddr -> t.

  Parameter dlTyp : dlTyp -> t.

  (** TODO(arjun): Only the 12 lower bits matter. If higher-order bits are
      non-zero, we might calculate incorrect intersections here too. *)
  Parameter dlVlan : dlVlan -> t.

  (** TODO(arjun): only lower 3 bits matter, similar to above. *)
  Parameter dlVlanPcp : dlVlanPcp -> t.

  Parameter ipSrc : nwAddr -> t.

  Parameter ipDst : nwAddr -> t.

  Parameter ipProto : nwProto -> t.

  Parameter inPort : port -> t.

  Parameter tcpSrcPort : tpPort -> t.

  Parameter tcpDstPort : tpPort -> t.

  Parameter udpSrcPort : tpPort -> t.

  Parameter udpDstPort : tpPort -> t.

  (** The [set*] functions update fields in the pattern if the update respects
      dependencies. If not, the pattern is unmodified. E.g., [setNwSrc] will
      update a pattern only if its frame-type is IP. *)
  Section Setters.

    Parameter setDlSrc : dlAddr -> t -> t.
    Parameter setDlDst : dlAddr -> t -> t.
(*    Parameter setDlVlan : NetworkPacket.dlVlan -> t -> t.
    Parameter setDlVlanPcp : NetworkPacket.dlVlanPcp -> t -> t.
    Parameter setNwSrc : nwAddr -> t -> t.
    Parameter setNwDst : nwAddr -> t -> t.
    Parameter setNwTos : nwTos -> t -> t.
    Parameter setTpSrc : tpPort -> t -> t.
    Parameter setTpDst : tpPort -> t -> t. *)

  End Setters.

  (** Pattern equivalence *)

  Definition equiv (pat1 pat2 : t) : Prop :=
    forall pt pk, 
      match_packet pt pk pat1 = match_packet pt pk pat2.

  Parameter equiv_is_Equivalence : Equivalence equiv.

  Instance Pattern_Equivalence : Equivalence equiv.
    apply equiv_is_Equivalence.
  Qed.

  Parameter beq_true_spec : forall p p',
    beq p p' = true ->
    p === p'.

  Parameter inter_comm : forall p p', inter p p' === inter p' p.

  Parameter inter_assoc : forall p p' p'',
    inter p (inter p' p'') === inter (inter p p') p''.

  Parameter is_empty_false_distr_l : forall x y,
    is_empty (inter x y) = false -> 
    is_empty x = false .

  Parameter is_empty_false_distr_r : forall x y,
    is_empty (inter x y) = false -> 
    is_empty y = false .

  Parameter is_empty_true_l : forall x y,
    is_empty x = true ->
    is_empty (inter x y) = true.

  Parameter is_empty_true_r : forall x y,
    is_empty y = true ->
    is_empty (inter x y) = true.

  Parameter is_match_false_inter_l :
    forall (pt : port) (pkt : packet) pat1 pat2,
      match_packet pt pkt pat1 = false ->
      match_packet pt pkt (inter pat1 pat2) = false.

  Parameter no_match_subset_r : forall k n t t',
    match_packet n k t' = false -> 
    match_packet n k (inter t t') = false.

  Parameter exact_match_inter : forall x y,
    is_exact x = true ->
    is_empty (inter x y) = false ->
    inter x y === x.

  Parameter all_spec : forall pt pk,
    match_packet pt pk all = true.

  Parameter exact_match_is_exact : forall pk pt,
    is_exact (exact_pattern pk pt) = true.

  Parameter exact_intersect : forall k n t,
    match_packet k n t = true ->
    inter (exact_pattern n k) t === exact_pattern n k.

  Parameter is_match_true_inter : forall pat1 pat2 pt pk,
    match_packet pt pk pat1 = true ->
    match_packet pt pk pat2 = true ->
    match_packet pt pk (inter pat1 pat2) = true.

  Parameter match_packet_spec : forall pt pk pat,
    match_packet pt pk pat = 
    negb (is_empty (inter (exact_pattern pk pt) pat)).

  Parameter all_is_not_empty : is_empty all = false.


End PATTERN.

Set Implicit Arguments.

Require Import Coq.Classes.Equivalence.
Require Import WordInterface.
Require Import Network.Packet.
Require Import OpenFlow.MessagesDef.

Local Open Scope equiv_scope.

Module Type PATTERN.

  Parameter t : Type.

  Parameter inter : t -> t -> t.

  Parameter all : t.

  Parameter empty : t.

  Parameter exact_pattern : packet -> portId -> t.

  Parameter is_empty : t -> bool.

  Parameter match_packet : portId -> packet -> t -> bool.

  Parameter is_exact : t -> bool.

  Parameter to_match : forall x, is_empty x = false -> of_match.

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

  Parameter inPort : portId -> t.

  Parameter tcpSrcPort : tpPort -> t.

  Parameter tcpDstPort : tpPort -> t.

  Parameter udpSrcPort : tpPort -> t.

  Parameter udpDstPort : tpPort -> t.

  (** Pattern equivalence *)

  Definition equiv (pat1 pat2 : t) : Prop :=
    forall pt pk, 
      match_packet pt pk pat1 = match_packet pt pk pat2.

  Axiom equiv_is_Equivalence : Equivalence equiv.

  Instance Pattern_Equivalence : Equivalence equiv.
    apply equiv_is_Equivalence.
  Qed.

  Axiom beq_true_spec : forall p p',
    beq p p' = true ->
    p === p'.

  Axiom inter_comm : forall p p', inter p p' === inter p' p.

  Axiom inter_assoc : forall p p' p'',
    inter p (inter p' p'') === inter (inter p p') p''.

  Axiom is_empty_false_distr_l : forall x y,
    is_empty (inter x y) = false -> 
    is_empty x = false .

  Axiom is_empty_false_distr_r : forall x y,
    is_empty (inter x y) = false -> 
    is_empty y = false .

  Axiom is_empty_true_l : forall x y,
    is_empty x = true ->
    is_empty (inter x y) = true.

  Axiom is_empty_true_r : forall x y,
    is_empty y = true ->
    is_empty (inter x y) = true.

  Axiom is_match_false_inter_l :
    forall (pt : portId) (pkt : packet) pat1 pat2,
      match_packet pt pkt pat1 = false ->
      match_packet pt pkt (inter pat1 pat2) = false.

  Axiom no_match_subset_r : forall k n t t',
    match_packet n k t' = false -> 
    match_packet n k (inter t t') = false.

  Axiom exact_match_inter : forall x y,
    is_exact x = true ->
    is_empty (inter x y) = false ->
    inter x y === x.

  Axiom all_spec : forall pt pk,
    match_packet pt pk all = true.

  Axiom exact_match_is_exact : forall pk pt,
    is_exact (exact_pattern pk pt) = true.

  Axiom exact_intersect : forall k n t,
    match_packet k n t = true ->
    inter (exact_pattern n k) t === exact_pattern n k.

  Axiom is_match_true_inter : forall pat1 pat2 pt pk,
    match_packet pt pk pat1 = true ->
    match_packet pt pk pat2 = true ->
    match_packet pt pk (inter pat1 pat2) = true.

End PATTERN.
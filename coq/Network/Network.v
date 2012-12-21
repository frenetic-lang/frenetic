Require Import Common.Utilities.
Require Import Coq.Lists.List.
Require Import Classes.EquivDec.
Require Import OpenFlow.Types.

Section Network.
  Definition host := nat.

  (* Inductive port : Type :=  *)
  (* | Port : nat -> port. *)

  (* Inductive switch :=  *)
  (* | Switch : nat -> switch *)
  (* | World : switch *)
  (* | Drop : switch. *)

  (* Coercion Switch : nat >-> switch. *)
  (* Coercion Port : nat >-> port. *)

  (* (* Mark: Don't like this name, come up with a better one *) *)
  (* Definition node := (host + switch) %type. *)

  (* Coercion swInj (sw : switch) : node := inr host sw. *)

  Inductive link := 
  | Link : Switch -> Port -> link.

  (* Require to be attached to switches? *)
  Definition host_map := host -> link.

  Definition graph := list (link * link).

  Definition topology := (host_map, graph).

  Definition path := list (link).
  Definition graph_search := link -> link -> graph -> option path.

  Parameter G : graph.
  Parameter H : host_map.
  Parameter H_inj : forall H1 H2, H H1 = H H2 -> H1 = H2.
  Parameter H_unique_ports : forall sw p h,
    ~ In (H h, Link sw p) G /\ ~ In (Link sw p, H h) G.
  
  Lemma eq_host_dec : forall h1 h2 : host, {h1=h2} + {h1<>h2}.
  Proof.
    repeat decide equality.
  Qed.

  (* Lemma eq_port_dec : forall p1 p2 : port, {p1=p2} + {p1<>p2}. *)
  (* Proof. *)
  (*   repeat decide equality. *)
  (* Qed. *)

  (* Lemma eq_switch_dec : forall sw1 sw2 : switch, {sw1=sw2} + {sw1<>sw2}. *)
  (* Proof. *)
  (*   repeat decide equality. *)
  (* Qed. *)

  Lemma eq_link_dec : forall l1 l2 : link, {l1=l2} + {l1<>l2}.
  Proof.
    repeat decide equality.
  Qed.

  Program Instance link_eq_eqdec : EqDec link eq := eq_link_dec.
  Program Instance host_eq_eqdec : EqDec host eq := eq_host_dec.

  Inductive Legal_path : path -> link -> link -> graph -> Prop :=
  | single_path_legal : forall s port1 port2 g,
    Legal_path [(Link s port1) ; (Link s port2)] (Link s port1) (Link s port2) g
  | trans_path_legal : forall a b c d g p p',
    In (b, c) g ->
    Legal_path p a b g ->
    Legal_path p' c d g ->
    Legal_path (p ++ p') a d g.

  Lemma Legal_path_non_empty :
    forall p n n' g,
      Legal_path p n n' g -> p <> [].
  Proof.
    red in |- *.
    intros.
    induction H0; util_crush; apply app_eq_nil in H1; intuition.
  Qed.

  Definition reachable host1 host2 g := exists p, Legal_path p (H host1) (H host2) g.

  Inductive loop_free_path : path -> Prop :=
  | empty_loop_free_path : loop_free_path []
  | unique_loop_free_path : forall (n : link) (p : path),
    loop_free_path p ->
    not (In n p) ->
    loop_free_path (n :: p).

End Network.

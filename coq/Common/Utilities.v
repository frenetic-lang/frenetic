Require Export Common.CpdtTactics.

Notation "[ ]" := nil : list_scope.
Require Import Lists.List.
Notation "[ a ; .. ; b ]" := (a :: .. (b :: []) ..) : list_scope.
Require Import Bool.Bool.

Lemma app_nil :
  forall A (ls : list A),
    (app ls []) = ls.
Proof. 
  crush.
Qed.

Hint Rewrite app_nil.

Lemma app_cons :
  forall A (e : A) l,
    [e] ++ l = e :: l.
Proof.
  crush.
Qed.

Hint Rewrite app_nil.

Lemma hd_error_app :
  forall A (e : A) l1 l2,
    hd_error l1 = value e ->
    hd_error (l1 ++ l2) = value e.
Proof.
  induction l1; crush. inversion H.
Qed.

Hint Rewrite hd_error_app.

Fixpoint last_error {A : Type} (l : list A) :=
  match l with
    | [] => error
    | [a] => value a
    | a::l' => last_error l'
  end.

Lemma last_error_error_is_nil :
  forall A (l : list A),
    last_error l = error -> l = [].
Proof.
  induction l; crush.
  destruct l. inversion H.
  apply IHl in H. inversion H.
Qed.
  
Lemma last_error_if_nil :
  forall A (l : list A),
    last_error l = error <-> l = [].
Proof.
  intros.
  split; crush. apply last_error_error_is_nil. assumption.
Qed.

Lemma last_error_non_nil :
  forall A a (l : list A),
    last_error l = value a -> l <> [].
Proof.
  red in |- *.
  intros.
  rewrite <- last_error_if_nil in H0. rewrite H in H0. inversion H0.
Qed.

Lemma last_error_app :
  forall A (e : A) l1 l2,
    last_error l2 = value e ->
    last_error (l1 ++ l2) = value e.
Proof.
  induction l1; crush. apply IHl1 in H. apply last_error_non_nil in H. crush.
  destruct (l1 ++ l2); crush.
Qed.


Definition override {A : Type} {B : Type} (f : A -> B) (g : A -> option B) (a' : A) :=
  match g a' with
    | None => f a'
    | Some b' => b'
  end.

Fixpoint override_list {A : Type} {B : Type} (f : A -> B) (us : list (A -> option B)) :=
  match us with
    | [] => f
    | g :: gs => override (override_list f gs) g
  end.

Fixpoint snoc {A} (elem : A) lst :=
  match lst with
    | [] => [ elem ]
    | x :: lst => x :: snoc elem lst
  end.

Lemma app_snoc :
  forall A (l1 : list A) (e : A) l2,
    app l1 (snoc e l2) = snoc e (app l1 l2).
Proof.
  intros. induction l1; crush. 
Qed.

Lemma override_list_override {A : Type} {B : Type} :
  forall (S : A -> B) u us,
    override_list (override S u) us = override_list S (snoc u us).
Proof.
  intros. induction us; crush. 
Qed.

Lemma override_empty :
  forall A B (F : A -> B),
    override_list F [] = F.
Proof. 
  crush. 
Qed.

Hint Rewrite override_empty.

Lemma snoc_not_nil :
  forall A (t : A) tr,
    snoc t tr <> [].
Proof. 
  induction tr; crush. 
Qed.

Lemma snoc_singleton :
  forall A (t : A) t' tr,
    snoc t tr = [t'] -> tr = [] /\ t = t'.
Proof.
  induction tr; crush; apply snoc_not_nil in H; crush.
Qed.

Lemma override_list_app :
  forall A B (f : A -> B) f' f'' a1 a2,
    f' = override_list f a1 ->
    f'' = override_list f' a2 ->
    f'' = override_list f (a2 ++ a1).
Proof.
  crush. induction a2; crush. 
Qed.

Lemma app_is_nil :
  forall A (l1 : list A) l2,
    [] = l1 ++ l2 -> l1 = [] /\ l2 = [].
Proof. 
  crush. induction l1; induction l2; crush. symmetry in H; apply app_eq_nil in H; crush.
Qed.

Lemma in_snoc :
  forall A a1 a2 (l1 : list A),
    In a1 l1 -> In a1 (snoc a2 l1).
Proof.
  intros. induction l1; crush. 
Qed.

Lemma in_snoc2 :
  forall A a1 (l1 : list A),
    In a1 (snoc a1 l1).
Proof.
  intros. induction l1; crush. 
Qed.

Lemma in_snoc3 :
  forall A a2 a1 (l1 : list A),
    a1 <> a2 ->
    In a1 (snoc a2 l1) ->
    In a1 l1.
Proof.
  induction l1; crush. 
Qed.

Lemma snoc_app:
  forall A (a1 : A) l1,
    snoc a1 l1 = l1 ++ [a1].
Proof.
  induction l1; crush. 
Qed.

Lemma snoc_cons :
  forall A (a1 : A) a2 l1,
    a1 :: snoc a2 l1 = snoc a2 (a1 :: l1).
Proof.
  induction l1; crush. 
Qed.

Lemma snoc_inj :
  forall A (a1 : A) l1 a2 l2,
    snoc a1 l1 = snoc a2 l2 ->
    a1 = a2 /\ l1 = l2.
Proof.
  induction l1; induction l2; crush. symmetry in H. apply snoc_not_nil in H. contradiction. 
  symmetry in H. apply snoc_not_nil in H. contradiction.
  apply snoc_not_nil in H. contradiction.
  apply snoc_not_nil in H. contradiction.
  specialize (IHl1 a2 l2). intuition. 
  specialize (IHl1 a2 l2). crush.
Qed.

Hint Rewrite rev_involutive.

Lemma snoc_rev :
  forall A (a : A) ls,
    snoc a ls = rev (a :: rev ls).
Proof.
  crush. induction ls; crush.
Qed.

Lemma snoc_inv :
  forall A (ls : list A),
    ls = [] \/ exists a, exists b, ls = snoc a b.
Proof.
  crush. induction ls; crush.
  right. exists a, []. crush.
  right. rewrite snoc_cons. exists x, (a :: x0). reflexivity.
Qed.

Lemma snoc_induction :
  forall A (P : list A -> Prop),
    (P []) ->
    (forall a ls, P ls -> P (snoc a ls)) ->
    forall ls, P ls.
Proof.
  intros. apply rev_ind. assumption. 
  intros. rewrite <- snoc_app. crush.
Qed.

Lemma snoc_double_induction :
  forall A (P : list A -> list A -> Prop),
    (P [] []) ->
    (forall a l1 l2, P l1 l2 -> P (snoc a l1) l2) ->      
    (forall a l1 l2, P l1 l2 -> P l1 (snoc a l2)) ->
    forall l1 l2, P l1 l2.
Proof.
  destruct l1 using snoc_induction; destruct l2 using snoc_induction; crush. 
Qed.


Ltac snoc_nil_tac :=
  match goal with
    | [ H : snoc _ _ = [] |-  _ ] => apply snoc_not_nil in H; contradiction
    | [ H : [] = snoc _ _ |-  _ ] => symmetry in H; apply snoc_not_nil in H; contradiction
  end.

Ltac snoc_singleton_tac :=
  match goal with
    | [ H : snoc _ ?A = [_] |- _ ] => 
      match goal with 
        | [ H' : ?A = [] |- _ ] => fail 2
        |_ => apply snoc_singleton in H
      end
    | [ H : [_] = snoc _ _ |- _ ] => symmetry in H; snoc_singleton_tac
  end.

Ltac snoc_inj_tac :=
  match goal with
    | [ H : snoc _ _ = snoc _ _  |- _ ] => apply snoc_inj in H; subst
  end.

Ltac in_snoc_tac :=
  match goal with
    | [ H : In ?A ?B |- In ?A (snoc _ ?B) ] => apply in_snoc; assumption
  end.

Ltac remember_clear H H' :=
  remember H as H';
    match goal with [ H'' : H' = H |- _ ] => clear H'' end.

Ltac snoc_tac' :=
  match goal with
    | [ |- [] <> snoc _ _ ] => intuition; snoc_nil_tac
    | [ |- snoc _ _ <> [] ] => intuition; snoc_nil_tac
  end.

Ltac snoc_tac :=
  snoc_nil_tac || snoc_tac' || snoc_inj_tac || snoc_singleton_tac || in_snoc_tac.

Ltac nil_cons_tac :=
  match goal with
    | [ H : [] = _ :: _ |- _ ] => discriminate
    | [ H : _ :: _ = [] |- _ ] => discriminate
  end.

Ltac util_crush :=
  repeat (snoc_tac || crush || nil_cons_tac).

Require Import Coq.Classes.Equivalence.
Require Import Coq.Classes.EquivDec.
Require Import Coq.Logic.Decidable.


Hint Extern 0 (?x === ?x) => reflexivity.
Hint Extern 1 (_ === _) => (symmetry; trivial; fail).
Hint Extern 1 (_ =/= _) => (symmetry; trivial; fail).

Lemma equiv_reflexive' : forall {A} `{EqDec A} (x : A),
  x === x.
Proof. 
  intros. apply equiv_reflexive. 
Qed.

Lemma equiv_symmetric' : forall {A} `{EqDec A} (x y : A),
  x === y ->
  y === x.
Proof.
  intros. apply equiv_symmetric; assumption. 
Qed.

Lemma equiv_transitive' : forall {A} `{EqDec A} (x y z : A),
  x === y ->
  y === z ->
  x === z.
Proof. 
  intros. eapply @equiv_transitive; eassumption. 
Qed.

Lemma equiv_decidable : forall {A} `{EqDec A} (x y : A),
  decidable (x === y).
Proof. 
  intros. unfold decidable. destruct (x == y); auto. 
Defined.

(* It is convenient to be able to use a single notation for decidable equality on types. This can naturally be done using a type class. However, the definition of EqDec in the EquivDec library is overly general for cases where the equality is eq: the extra layer of abstraction provided by abstracting over the equivalence relation gets in the way of smooth reasoning. To get around this, we define a version of that class where the equivalence relation is hard-coded to be eq. *)

(* Implementation note: One should not declare an instance for EqDec_eq A directly. First, declare an instance for @EqDec A eq eq_equivalence. Second, let class inference build an instance for EqDec_eq A using the instance declaration below. *)

(* Implementation note (BEA): Specifying eq_equivalence explicitly is important. Following Murphy's Law, if type class inference can find multiple ways of inferring the @Equivalence A eq argument, it will do so in the most inconvenient way possible. Additionally, I choose to infer EqDec_eq instances from EqDec instances because the standard library already defines instances for EqDec.  *)
Class EqDec_eq (A : Type) :=
eq_dec : forall (x y : A), {x = y} + {x <> y}.

Instance EqDec_eq_of_EqDec {A} `(@EqDec A eq eq_equivalence) : EqDec_eq A.
Proof. 
  trivial. 
Defined.

Notation " x == y " := (eq_dec (x :>) (y :>)) (no associativity, at level 70) : equiv_scope.

Definition equiv_decb' {A} `{EqDec_eq A} (x y : A) : bool :=
  if x == y then true else false.

Definition nequiv_decb' {A} `{EqDec_eq A} (x y : A) : bool :=
  negb (equiv_decb' x y).

Infix "==b" := equiv_decb' (no associativity, at level 70).

Infix "<>b" := nequiv_decb' (no associativity, at level 70).

Lemma eq_option_dec : forall {A} `{EqDec_eq A} (x y : option A),
  {x = y} + {x <> y}.
Proof.
  repeat decide equality.
Qed.

Lemma eq_prod_dec : forall {A B} `{EqDec_eq A} `{EqDec_eq B} (x : A*B) (y : A*B),
  {x = y} + {x <> y}.
Proof.
  repeat decide equality.
Qed.

Instance EqDec_of_prod_EqDec {A B} `{EqDec_eq A} `{EqDec_eq B} : EqDec (A*B) eq := eq_prod_dec.

(* Lemma eq_prod3_dec : forall {A B C} `{EqDec_eq A} `{EqDec_eq B} `{EqDec_eq C} (x : A*B*C) (y : A*B*C), *)
(*   {x = y} + {x <> y}. *)
(* Proof. *)
(*   repeat decide equality. *)
(* Qed. *)

(* Instance EqDec_of_prod3_EqDec {A B C} `{EqDec_eq A} `{EqDec_eq B} `{EqDec_eq C} : EqDec (A*B*C) eq := eq_prod3_dec. *)

Definition updateFun {A : Type} {B : Type} `{eqA : EqDec_eq A} (f : A -> B) (a : A) (b : B) (a' : A) :=
  if a ==b a' then b else f a'.

(* Definition updateFun {B} (f : port -> B) (a : port) (b : B) (a' : port) := *)
(*   if port_eq a a' then b else f a'. *)

Notation "f ⟦ a |-> b ⟧ " :=
  (updateFun f a b) (at level 0).

Lemma update_fun_same_arg {A : Type} {B : Type} `{eqA : EqDec_eq A} :
  forall (f : A -> B) a (b : B),
    updateFun f a b a = b.
Proof.
  intros. unfold updateFun. unfold equiv_decb'. destruct eq_dec; crush.
Qed.

(* Hint Rewrite update_fun_same_arg : cpdt. *)

Lemma update_fun_diff_arg {A : Type} {B : Type} `{eqA : EqDec_eq A} :
  forall f a (b : B) a',
    a<>a'-> updateFun f a b a' = f a'.
Proof.
  crush. unfold updateFun. case_eq (a ==b a'); crush. unfold equiv_decb' in H0. destruct eq_dec; crush.
Qed.

(* Hint Rewrite update_fun_diff_arg : cpdt. *)

Definition append_function {A : Type} {B : Type} `{eqA : EqDec_eq A} (f : A -> list B) (a : A) (b : B) (a' : A) :=
  if a ==b a' then b :: f a' else f a'.

Definition extend {A : Type} {B : Type} (f : A -> list B) (g : A -> list B) (a : A) :=
  f a ++ g a.

Definition extend2 {A : Type} {B : Type} {C : Type} (f : A -> list B*list C) (g : A -> list B*list C) (a : A) :=
  (fst (f a) ++ fst (g a), snd (f a) ++ snd (g a)).

Fixpoint mem {A} `{EqDec_eq A} (a : A) (l : list A) :=
  match l with
    | [] => false
    | b :: m => if a ==b b then true else mem a m
  end.

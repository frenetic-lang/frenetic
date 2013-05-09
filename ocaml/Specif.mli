type __ = Obj.t

type 'a coq_sig =
  'a
  (* singleton inductive, whose constructor was exist *)

val sig_rect : ('a1 -> __ -> 'a2) -> 'a1 -> 'a2

val sig_rec : ('a1 -> __ -> 'a2) -> 'a1 -> 'a2

type 'a sig2 =
  'a
  (* singleton inductive, whose constructor was exist2 *)

val sig2_rect : ('a1 -> __ -> __ -> 'a2) -> 'a1 sig2 -> 'a2

val sig2_rec : ('a1 -> __ -> __ -> 'a2) -> 'a1 sig2 -> 'a2

type ('a, 'p) sigT =
| Coq_existT of 'a * 'p

val sigT_rect : ('a1 -> 'a2 -> 'a3) -> ('a1, 'a2) sigT -> 'a3

val sigT_rec : ('a1 -> 'a2 -> 'a3) -> ('a1, 'a2) sigT -> 'a3

type ('a, 'p, 'q) sigT2 =
| Coq_existT2 of 'a * 'p * 'q

val sigT2_rect : ('a1 -> 'a2 -> 'a3 -> 'a4) -> ('a1, 'a2, 'a3) sigT2 -> 'a4

val sigT2_rec : ('a1 -> 'a2 -> 'a3 -> 'a4) -> ('a1, 'a2, 'a3) sigT2 -> 'a4

val proj1_sig : 'a1 -> 'a1

val projT1 : ('a1, 'a2) sigT -> 'a1

val projT2 : ('a1, 'a2) sigT -> 'a2

val sig_of_sigT : ('a1, __) sigT -> 'a1

val sigT_of_sig : 'a1 -> ('a1, __) sigT

val sumbool_rect : (__ -> 'a1) -> (__ -> 'a1) -> bool -> 'a1

val sumbool_rec : (__ -> 'a1) -> (__ -> 'a1) -> bool -> 'a1

val sumor_rect : ('a1 -> 'a2) -> (__ -> 'a2) -> 'a1 option -> 'a2

val sumor_rec : ('a1 -> 'a2) -> (__ -> 'a2) -> 'a1 option -> 'a2

val coq_Choice : ('a1 -> 'a2) -> ('a1 -> 'a2)

val coq_Choice2 : ('a1 -> ('a2, 'a3) sigT) -> ('a1 -> 'a2, 'a1 -> 'a3) sigT

val bool_choice : ('a1 -> bool) -> ('a1 -> bool)

val dependent_choice : ('a1 -> 'a1) -> 'a1 -> (int -> 'a1)

type 'a coq_Exc = 'a option

val value : 'a1 -> 'a1 option

val error : 'a1 option

val except : __ -> 'a1

val absurd_set : __ -> 'a1


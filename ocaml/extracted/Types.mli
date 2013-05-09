open Datatypes
open List0

type 'a coq_Eqdec = 'a -> 'a -> bool

val second : ('a2 -> 'a3) -> ('a1 * 'a2) -> 'a1 * 'a3

type 'a coq_Eq =
  'a -> 'a -> bool
  (* singleton inductive, whose constructor was Build_Eq *)

val coq_Eq_rect : (('a1 -> 'a1 -> bool) -> 'a2) -> 'a1 coq_Eq -> 'a2

val coq_Eq_rec : (('a1 -> 'a1 -> bool) -> 'a2) -> 'a1 coq_Eq -> 'a2

val eqdec : 'a1 coq_Eq -> 'a1 -> 'a1 -> bool

val beqdec : 'a1 coq_Eq -> 'a1 -> 'a1 -> bool

val coq_Eq_nat : int coq_Eq

val option_eq : 'a1 coq_Eq -> 'a1 option -> 'a1 option -> bool

val coq_Eq_option : 'a1 coq_Eq -> 'a1 option coq_Eq

val pair_eq : 'a1 coq_Eq -> 'a2 coq_Eq -> ('a1 * 'a2) -> ('a1 * 'a2) -> bool

val coq_Eq_pair : 'a1 coq_Eq -> 'a2 coq_Eq -> ('a1 * 'a2) coq_Eq

val list_eq : 'a1 coq_Eq -> 'a1 list -> 'a1 list -> bool

val coq_Eq_list : 'a1 coq_Eq -> 'a1 list coq_Eq

val concat_map : ('a1 -> 'a2 list) -> 'a1 list -> 'a2 list

val filter_map_body : ('a1 -> 'a2 option) -> 'a1 -> 'a2 list -> 'a2 list

val filter_map : ('a1 -> 'a2 option) -> 'a1 list -> 'a2 list

val intersperse : 'a1 -> 'a1 list -> 'a1 list


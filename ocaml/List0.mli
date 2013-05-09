open Datatypes
open Specif

type __ = Obj.t

val hd : 'a1 -> 'a1 list -> 'a1

val hd_error : 'a1 list -> 'a1 option

val tl : 'a1 list -> 'a1 list

module ListNotations : 
 sig 
  
 end

val destruct_list : 'a1 list -> ('a1, 'a1 list) sigT option

val in_dec : ('a1 -> 'a1 -> bool) -> 'a1 -> 'a1 list -> bool

val nth : int -> 'a1 list -> 'a1 -> 'a1

val nth_ok : int -> 'a1 list -> 'a1 -> bool

val nth_in_or_default : int -> 'a1 list -> 'a1 -> bool

val nth_error : 'a1 list -> int -> 'a1 coq_Exc

val nth_default : 'a1 -> 'a1 list -> int -> 'a1

val remove : ('a1 -> 'a1 -> bool) -> 'a1 -> 'a1 list -> 'a1 list

val last : 'a1 list -> 'a1 -> 'a1

val removelast : 'a1 list -> 'a1 list

val exists_last : 'a1 list -> ('a1 list, 'a1) sigT

val count_occ : ('a1 -> 'a1 -> bool) -> 'a1 list -> 'a1 -> int

val rev : 'a1 list -> 'a1 list

val rev_append : 'a1 list -> 'a1 list -> 'a1 list

val rev' : 'a1 list -> 'a1 list

val list_eq_dec : ('a1 -> 'a1 -> bool) -> 'a1 list -> 'a1 list -> bool

val map : ('a1 -> 'a2) -> 'a1 list -> 'a2 list

val flat_map : ('a1 -> 'a2 list) -> 'a1 list -> 'a2 list

val fold_left : ('a1 -> 'a2 -> 'a1) -> 'a2 list -> 'a1 -> 'a1

val fold_right : ('a2 -> 'a1 -> 'a1) -> 'a1 -> 'a2 list -> 'a1

val list_power : 'a1 list -> 'a2 list -> ('a1 * 'a2) list list

val existsb : ('a1 -> bool) -> 'a1 list -> bool

val forallb : ('a1 -> bool) -> 'a1 list -> bool

val filter : ('a1 -> bool) -> 'a1 list -> 'a1 list

val find : ('a1 -> bool) -> 'a1 list -> 'a1 option

val partition : ('a1 -> bool) -> 'a1 list -> 'a1 list * 'a1 list

val split : ('a1 * 'a2) list -> 'a1 list * 'a2 list

val combine : 'a1 list -> 'a2 list -> ('a1 * 'a2) list

val list_prod : 'a1 list -> 'a2 list -> ('a1 * 'a2) list

val firstn : int -> 'a1 list -> 'a1 list

val skipn : int -> 'a1 list -> 'a1 list

val seq : int -> int -> int list

val coq_Forall_rect :
  'a2 -> ('a1 -> 'a1 list -> __ -> 'a2) -> 'a1 list -> 'a2


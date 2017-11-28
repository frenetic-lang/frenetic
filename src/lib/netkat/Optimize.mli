open Syntax

val mk_and : pred -> pred -> pred
val mk_or : pred -> pred -> pred
val mk_not :  pred -> pred
val mk_filter : pred -> policy
val mk_union : policy -> policy -> policy
val mk_seq : policy -> policy -> policy
val mk_star : policy -> policy
val specialize_pred : switchId -> pred -> pred
val specialize_policy : switchId -> policy -> policy
val mk_big_and : pred list -> pred
val mk_big_or : pred list -> pred
val mk_big_union : policy list -> policy
val mk_big_seq : policy list -> policy
val norm_policy : policy -> policy
val flatten_union : policy -> policy list
(** Warning: will re-order the elements in a union *)

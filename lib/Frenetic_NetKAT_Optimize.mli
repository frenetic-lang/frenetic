open Frenetic_NetKAT

val mk_and : pred -> pred -> pred
val mk_or : pred -> pred -> pred
val mk_not :  pred -> pred
val mk_filter : pred -> policy
val mk_mod : header_val -> policy
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


(** {Syntax} **)
module Nk : sig
  (* ordered by precedence, see http://caml.inria.fr/pub/docs/manual-caml-light/node4.9.html  *)
  val ( !! ) : header_val -> policy
  val ( ?? ) : header_val -> policy
  val ( ??? ) : header_val -> pred
  val port : int32 -> header_val
  val star : policy -> policy
  val ( * ) : int32 -> int32 -> int32
  val ( / ) : int32 -> int32 -> int32 * int32
  val ( --> ) : int64 * int32 -> int64 * int32 -> policy
  val not : pred -> pred
  val ( >> ) : policy -> policy -> policy
  val ( && ) : pred -> pred -> pred
  val ( || ) : policy -> policy -> policy
  val ( or ) : pred -> pred -> pred

  val ip4Src : int32 * int32 -> header_val
  val ip4Dst : int32 * int32 -> header_val
end

(** OpenFlow 1.0: Helps determine valid modification compositions.

	OpenFlow modification actions are imperative, which makes it difficult to
	calculate the parallel composition of two action sequences. For example,
	the following parallel composition is not realizable in OpenFlow 1.0:

	[[SetDlSrc 10, Output 5] + [Output 1]]

  We need to flatten the composition into a sequence. The sequence needs to
  undo the effect of [SetDlSrc 10] before executing [Output 1], but that is
  not possible to do.
*)

(** A bit-vector that represents which files are modified by an action
    sequence. *)
type t

(** No modifications *)
val none : t


val	dlSrc : t
val dlDst : t
val dlVlan : t
val dlVlanPcp : t
val nwSrc : t
val nwDst : t
val tpSrc : t
val tpDst : t

val seq : t -> t -> t

(** [par m1 m2] may fail with
    [Invalid_argument "unrealizable parallel composition"] *)
val par : t -> t -> t


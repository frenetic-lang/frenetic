type t

val of_local_pol : ?env:Fdd.Field.Env.t -> Syntax.policy -> t
val to_fdd : t -> Local_compiler.FDD.t

(** operations  *)
val of_pred : Fdd.Field.Env.t -> Syntax.pred -> t
val of_mod : Fdd.Field.Env.t -> Syntax.header_val -> t
val seq : t -> t -> t
val union : t -> t -> t
val star : t -> t


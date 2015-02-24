(* For debugging *)
type t
val of_policy : ?dedup:bool -> ?ing:NetKAT_Types.pred -> NetKAT_Types.policy -> t
val to_local : NetKAT_FDD.Field.t -> t -> NetKAT_LocalCompiler.t
val to_dot : t -> string

(* Global compiler *)
val compile : NetKAT_Types.policy -> NetKAT_LocalCompiler.t

type t

(** [gensym ()] produces a symbol that prints as [gensymN] where [N]
    is a number. Each symbol returned is physically distinct from all
    others. *)
val gensym : unit -> t

(** [to_string (gensym_printing str) = str]. However, applying
    [gensym_printing] twice to the same string produces two physically
    distinct symbols. *)
val gensym_printing : string -> t

val to_string : t -> string

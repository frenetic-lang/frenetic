module WorkList(K: Set.OrderedType) : sig
  type t
  val add : K.t -> t -> t
  val singleton : K.t -> t
  val is_empty : t -> bool
  val hd : t -> K.t
  val tl : t -> t
  val all_seen_items : t -> K.t list
end

val check_equivalent : Frenetic_Decide_Ast.Term.t -> Frenetic_Decide_Ast.Term.t -> bool

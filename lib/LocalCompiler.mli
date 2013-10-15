module type S = sig
  type policy
  type pred
  type header_val_map

  module Action : sig
    type t = header_val_map
    module Set : Set.S with type elt = t
    val set_to_string : Set.t -> string
    val seq_acts : t -> Set.t -> Set.t
    val id : Set.t
    val drop : Set.t
    val set_to_netkat : Set.t -> policy
  end

  module Pattern : sig
    type t = header_val_map
    module Set : Set.S with type elt = t
    val set_to_string : Set.t -> string
    val to_string : t -> string
    val tru : t
    val apply_act : t -> Action.t -> t
    val seq_pat : t -> t -> t option
    val seq_act_pat : t -> Action.t -> t -> t option
    val set_to_netkat : Set.t -> pred
    val to_netkat : t -> pred
  end

  module Atom : sig
    type t = Pattern.Set.t * Pattern.t
    module Map : Map.S with type key = t
    val to_string : t -> string
    val seq_atom : t -> t -> t option
    val seq_act_atom : t -> Action.t -> t -> t option
    val tru : t
    val fls : t
  end 

  module Local : sig
    type t = Action.Set.t Atom.Map.t
    val of_policy : policy -> t
    val to_netkat : t -> policy
  end
end

module Make
  (Headers : Semantics.HEADERS) 
  (Syntax : Semantics.S with type header = Headers.header
                         and type header_val = Headers.value
                         and type payload = Headers.payload) : S
  with type policy = Syntax.policy
   and type pred = Syntax.pred
   and type header_val_map = Syntax.header_val_map

module Local : sig
  type t
  val of_policy : NetKAT_Types.policy -> t
  val to_netkat : t -> NetKAT_Types.policy
end

module RunTime : sig 
  (* intermediate form *)
  type i 
  val compile : NetKAT_Types.policy -> i
  val decompile : i -> NetKAT_Types.policy
  val to_table : SDN_Types.fieldVal -> i -> SDN_Types.flowTable 
end

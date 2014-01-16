module Action : sig
  type t = NetKAT_Types.header_val_map
  module Set : Set.S with type elt = t
  type group = Set.t list
  val to_string : t -> string
  val set_to_string : Set.t -> string
  val group_to_string : group -> string
  val group_compare : group -> group -> int
  val group_crossproduct : group -> group -> group
  val group_union : group -> group -> group
  val seq_acts : t -> Set.t -> Set.t
  val id : Set.t
  val drop : Set.t
  val group_to_netkat : group -> NetKAT_Types.policy
end

module Pattern : sig
  type t = NetKAT_Types.header_val_map
  module Set : Set.S with type elt = t
  val set_to_string : Set.t -> string
  val to_string : t -> string
  val tru : t
  val is_tru : t -> bool
  val seq_pat : t -> t -> t option
  val seq_act_pat : t -> Action.t -> t -> t option
  val set_to_netkat : Set.t -> NetKAT_Types.pred
  val to_netkat : t -> NetKAT_Types.pred
end
  
module Atom : sig
  type t = Pattern.Set.t * Pattern.t
  module Map : Map.S with type key = t
  module Set : Set.S with type elt = t
  val to_string : t -> string
  val set_to_string : Set.t -> string
  val seq_atom : t -> t -> t option
  val seq_act_atom : t -> Action.t -> t -> t option
  val diff_atom : t -> t -> Set.t
  val tru : t
  val fls : t
end 
  
module Local : sig
  type t = Action.group Atom.Map.t
  val of_policy : SDN_Types.fieldVal -> NetKAT_Types.policy -> t
  val to_netkat : t -> NetKAT_Types.policy
end

module RunTime : sig 
  (* intermediate form *)
  type i 
  val compile : SDN_Types.fieldVal -> NetKAT_Types.policy -> i
  val decompile : i -> NetKAT_Types.policy
  val to_table : i -> SDN_Types.flowTable
end

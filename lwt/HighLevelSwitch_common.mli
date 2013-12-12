(** Operations that are necessary for both OpenFlow 1.0 and OpenFlow 1.3 *)

(** Version-specific bits. *)
module type VERSION_SPECIFIC = sig

  (* Atomic actions for some version of OpenFlow. *)
  type of_action

  (* The type of ports for some version of OpenFlow. *)
  type of_portId

  val from_action : of_portId option -> SDN_Types.action ->
    ModComposition.t * of_action

end

module type S = sig

  type of_action
  type of_portId

  val flatten_par : of_portId option -> SDN_Types.par -> of_action list

end

module Make : functor (OF : VERSION_SPECIFIC) ->
  S with type of_action = OF.of_action
     and type of_portId = OF.of_portId

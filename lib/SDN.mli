(** A uniform interface for programming switches that can use both OpenFlow 1.0
	and OpenFlow 1.3 as its back-end. *)

(** [accept_switches port] accepts new switches connecting to [port]. It returns
    an Lwt wakener which when called stops listening for new switches. *)
val accept_switches : int -> 
  (unit Lwt.u * SDN_types.switchFeatures Lwt_stream.t) Lwt.t

 (** [setup_flow_table sw tbl] returns after [sw] is configured to implement 
  [tbl]. [setup_flow_table] fails if [sw] runs a version of OpenFlow that
  does not support the features that [tbl] requires. *)
val setup_flow_table : SDN_types.switchId -> SDN_types.flowTable -> unit Lwt.t

val flow_stats_request : SDN_types.switchId -> SDN_types.pattern
  -> SDN_types.flowStats list Lwt.t

val packet_in : SDN_types.switchId -> SDN_types.pktIn Lwt_stream.t

val packet_out : SDN_types.switchId -> SDN_types.payload
  -> SDN_types.action -> unit Lwt.t
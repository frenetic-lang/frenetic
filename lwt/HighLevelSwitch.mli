open SDN_Types

type t
(** [setup_flow_table sw tbl] returns after [sw] is configured to implement 
    [tbl]. [setup_flow_table] fails if [sw] runs a version of OpenFlow that
    does not support the features that [tbl] requires. *)
val setup_flow_table : t -> flowTable -> unit Lwt.t
val flow_stats_request : t -> pattern -> flowStats list Lwt.t
val packet_in : t -> pktIn Lwt_stream.t
val packet_out : t -> payload -> par -> unit Lwt.t
val disconnect : t -> unit Lwt.t
val features : t -> switchFeatures

val initialize : Lwt_unix.file_descr -> t option Lwt.t

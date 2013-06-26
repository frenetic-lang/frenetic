(** Functions that an Ox controller can use to send OpenFlow messages. *)
open OpenFlow0x01_Core
open OpenFlow0x01_Stats

(** [send_packet_out sw xid pkt] sends a [packetOut] message with
    transaction ID [xid] to switch [sw]. *)
val send_packet_out : switchId -> xid -> packetOut -> unit

(** [send_flow_mod sw xid mod] sends a [flowMod] message with
    transaction ID [xid] to switch [sw]. *)
val send_flow_mod : switchId -> xid -> flowMod -> unit

(** [send_barrier_request sw xid] sends a barrier request to switch [sw] with
    transaction ID [xid]. *)
val send_barrier_request : switchId -> xid -> unit

(** [send_stats_request sw xid req] sends a stats request
    message with transaction ID [xid] to switch [sw]. *)
val send_stats_request : switchId -> xid -> request -> unit

(** [timeout x callback] calls the [callback] after [x] seconds
    have elapsed.

    Since [x] is a floating-point value, you can specify fractions of
    a second. *)
val timeout : float -> (unit -> unit) -> unit

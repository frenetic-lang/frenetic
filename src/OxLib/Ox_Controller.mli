open OpenFlow0x01

module OxPlatform : sig
  val send_packet_out : switchId -> xid -> PacketOut.t -> unit
  val send_flow_mod : switchId -> xid -> FlowMod.t -> unit
  val send_barrier_request : switchId -> xid -> unit
  val send_stats_request : switchId -> xid -> StatsRequest.t -> unit
  val timeout : float -> (unit -> unit) -> unit
end

module type OXMODULE = sig
  val switch_connected : switchId -> unit
  val switch_disconnected : switchId -> unit
  val packet_in : switchId -> xid -> PacketIn.t -> unit
  val barrier_reply : switchId -> xid -> unit
  val stats_reply : switchId -> xid -> StatsReply.t -> unit
  val port_status : switchId -> xid -> PortStatus.t -> unit
end

module Make :functor (OxModule:OXMODULE) ->
sig
  val start_controller : unit -> unit Lwt.t
end

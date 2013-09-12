open OpenFlow0x01
open Packet
open NetCore_Types

val start_controller : 
  (switchId * portId * bytes) Lwt_stream.t 
  -> pol NetCore_Stream.t 
  -> unit Lwt.t

val start_consistent_controller : 
  (switchId * portId * bytes) Lwt_stream.t 
  -> pol NetCore_Stream.t
  -> NetCore_Topology.Topology.t
  -> unit Lwt.t

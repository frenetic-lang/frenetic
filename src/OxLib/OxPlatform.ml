open Printf
open Packet
open OpenFlow0x01_Core
open OpenFlow0x01
open Message
open OxShared

let send_packet_out sw xid pktOut =
  defer (Some (sw, xid, PacketOutMsg pktOut))
	  
let send_flow_mod sw xid flowMod =
  defer (Some (sw, xid, FlowModMsg flowMod))
	  
let send_stats_request sw xid req =
  defer (Some (sw, xid, StatsRequestMsg req))
    
let send_barrier_request sw xid =
  defer (Some (sw, xid, BarrierRequest))
    
(* TODO(arjun): I'm not happy about this. I want an exception to terminate
   the right swich, unless we have exceptions kill the controller. *)
let timeout (n : float) (thk : unit -> unit) : unit = 
  Lwt.async 
    (fun () -> Lwt_unix.sleep n >> munge_exns thk)

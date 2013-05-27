open Printf
open Packet
open OpenFlow0x01

module Log = Frenetic_Log

let (<&>) = Lwt.(<&>)

module type OXPLATFORM = 
sig
  val packetOut : switchId -> packetOut -> unit Lwt.t
  val flowMod : switchId -> flowMod -> unit Lwt.t
  val statsRequest : switchId -> statsRequest -> unit Lwt.t
end

module type OXHANDLER = 
sig
  val switchConnected : switchId -> unit Lwt.t
  val switchDisconnected : switchId -> unit Lwt.t
  val packetIn : switchId -> packetIn -> unit Lwt.t
  val statsReply : switchId -> statsReply -> unit Lwt.t
end

module MakeOxPlatform(Platform:OpenFlow0x01.PLATFORM) = struct
  let xid = ref Int32.zero 

  let packetOut sw pktOut = 
    xid := Int32.succ !xid;
    Platform.send_to_switch sw !xid (PacketOutMsg pktOut)  

  let flowMod sw flowMod = 
    xid := Int32.succ !xid;
    Platform.send_to_switch sw !xid (FlowModMsg flowMod)  

  let statsRequest sw req =
    xid := Int32.succ !xid;
    Platform.send_to_switch sw !xid (StatsRequestMsg req)
end

module Make (Platform:OpenFlow0x01.PLATFORM) (Handler:OXHANDLER) = 
struct
  let rec switch_thread sw = 
    try_lwt
      lwt msg = Platform.recv_from_switch sw in 
      match msg with 
        | (_,PacketInMsg pktIn) -> 
	  Log.printf "Ox_controller" "got packetin\n%!";
          Handler.packetIn sw pktIn >>
          switch_thread sw 
        | (_, StatsReplyMsg rep) ->
          Handler.statsReply sw rep >>
          switch_thread sw
        | _ -> 
          switch_thread sw 
    with Platform.SwitchDisconnected sw -> 
      Handler.switchDisconnected sw 

  let rec accept_switches () = 
    lwt feats = Platform.accept_switch () in 
    let sw = feats.switch_id in 
    let _ = Log.printf "Ox_Controller" "switch %Ld connected\n%!" sw in 
    Handler.switchConnected sw <&>
    switch_thread sw <&> 
    accept_switches ()

  let start_controller = accept_switches
end

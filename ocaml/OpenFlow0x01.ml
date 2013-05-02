module Parser = OpenFlow0x01_Parser

module Types = struct
  include OpenFlow0x01Types

  let match_all = {
    matchDlSrc = None;
    matchDlDst = None;
    matchDlTyp = None;
    matchDlVlan = None;
    matchDlVlanPcp = None;
    matchNwSrc = None; 
    matchNwDst = None;
    matchNwProto = None;
    matchNwTos = None;
    matchTpSrc = None;
    matchTpDst = None;
    matchInPort = None
  }

  let delete_all_flows = 
    FlowModMsg {
      mfModCmd = DeleteFlow;
      mfMatch = match_all;
      mfPriority = 0;
      mfActions = [];
      mfCookie = 0L;
      mfIdleTimeOut = Permanent;
      mfHardTimeOut = Permanent;
      mfNotifyWhenRemoved = false;
      mfApplyToPacket = None;
      mfOutPort = None;
      mfCheckOverlap = false
    }

end

module type PLATFORM = sig
  open Types
  exception SwitchDisconnected of switchId 
  val send_to_switch : switchId -> xid -> message -> unit Lwt.t
  val recv_from_switch : switchId -> (xid * message) Lwt.t
  val accept_switch : unit -> features Lwt.t
end

module Platform = OpenFlow0x01_Platform

module TestPlatform = OpenFlow0x01_TestPlatform


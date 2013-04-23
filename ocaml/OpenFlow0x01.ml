module Parser = OpenFlow0x01_Parser

module Types = OpenFlow0x01Types

module type PLATFORM = sig
  open Types
  exception SwitchDisconnected of switchId 
  val send_to_switch : switchId -> xid -> message -> unit Lwt.t
  val recv_from_switch : switchId -> (xid * message) Lwt.t
  val accept_switch : unit -> features Lwt.t
end

module Platform = OpenFlow0x01_Platform

module TestPlatform = OpenFlow0x01_TestPlatform


open Core.Std
open Async.Std

module Net = Async_NetKAT.Net

module Controller = Async_OpenFlow.OpenFlow0x01.Controller
module Stage = Async_OpenFlow.Stage
module SDN = SDN_Types
module M = OpenFlow0x01.Message
module LC = NetKAT_LocalCompiler

type switchId = SDN_Types.switchId

module SwitchMap = Map.Make(Int64)
module XidMap = Map.Make(Int32)

module Log = Async_OpenFlow.Log

let tags = [("openflow", "controller")]

type t = {
  ctl : Controller.t;
  dis : Discovery.t;
  nib : Net.Topology.t ref;
  mutable prev_order : LC.order;
  mutable order : LC.order;
  mutable repr : LC.t;
  mutable edge : (SDN_Types.flow*int) list SwitchMap.t;
}

let send t c_id msg =
  Controller.send t c_id msg
  >>| function
    | `Sent _ -> ()
    | `Drop exn -> raise exn

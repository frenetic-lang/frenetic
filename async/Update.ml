open Core.Std
open Async.Std

module Controller = Async_OpenFlow.OpenFlow0x01.Controller

module Net = Async_NetKAT.Net
module SDN = SDN_Types

module type S = sig
  val bring_up_switch : Controller.t -> SDN.switchId -> NetKAT_Types.policy -> unit Deferred.t
  val implement_policy : Controller.t -> Net.Topology.t -> NetKAT_Types.policy -> unit Deferred.t
end

module BestEffort : S = struct
  module M = OpenFlow0x01.Message

  let install_flows_for (t : Controller.t) c_id table =
    let to_flow_mod p f = M.FlowModMsg (SDN_OpenFlow0x01.from_flow p f) in
    let priority = ref 65536 in
    Deferred.List.iter table ~f:(fun flow ->
      decr priority;
      Controller.send t c_id (0l, to_flow_mod !priority flow)
      >>| function
        | `Drop exn -> raise exn
        | `Sent _   -> ())

  let delete_flows_for (t :Controller.t) c_id =
    let delete_flows = M.FlowModMsg OpenFlow0x01_Core.delete_all_flows in
    Controller.send t c_id (5l, delete_flows)
    >>| function
      | `Drop exn -> raise exn
      | `Sent _   -> ()

  let bring_up_switch (t : Controller.t) (sw_id : SDN.switchId) (policy : NetKAT_Types.policy) =
    let table = NetKAT_LocalCompiler.(to_table (compile sw_id policy)) in
    let c_id = Controller.client_id_of_switch t sw_id in
    delete_flows_for t c_id >>= fun () ->
    install_flows_for t c_id table

  let implement_policy (t : Controller.t) (nib : Net.Topology.t) (policy : NetKAT_Types.policy) =
    Deferred.List.iter (TUtil.switch_ids nib) (fun sw_id ->
      bring_up_switch t sw_id policy)
end

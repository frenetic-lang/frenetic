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

(* NB: This module contains internal state. It is here for code organization and
 * not for code reuse. You can only use this module in one place in each
 * program.
 *)
module PerPacketConsistent = struct

  let specialize_action ver internal_ports actions =
    let open SDN in
    List.fold_right actions ~init:[] ~f:(fun action acc ->
      begin match action with
      | Output (Physical   pt) ->
        if not (Net.Topology.PortSet.mem pt internal_ports) then
          [Modify (SetVlan None)]
        else
          [Modify (SetVlan (Some ver))]
      | Output (Controller n ) ->
        [Modify (SetVlan None)]
      | Output _               -> assert false
      | _                      -> acc
      end @ (action :: acc))

  let specialize_edge_to (ver : int) internal_ports (table : SDN.flowTable) =
    let open SDN in
    let vlan_none = 65535 in
    List.filter_map table ~f:(fun flow ->
      begin match flow.pattern.Pattern.inPort with
      | Some pt when Net.Topology.PortSet.mem pt internal_ports ->
        None
      | _ ->
        Some { flow with
          pattern = { flow.pattern with Pattern.dlVlan = Some vlan_none }
        ; action  = List.map flow.action ~f:(fun x ->
            List.map x ~f:(specialize_action ver internal_ports))
        }
      end)

  let specialize_internal_to (ver : int) internal_ports (table : SDN.flowTable) =
    let open SDN in
    List.map table ~f:(fun flow ->
      { flow with
        pattern = { flow.pattern with Pattern.dlVlan = Some ver }
      ; action  = List.map flow.action ~f:(fun x ->
          List.map x ~f:(specialize_action ver internal_ports))
      })

end

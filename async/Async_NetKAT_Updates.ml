open Core.Std
open Async.Std
open Async_NetKAT_Controller_Common

type edge = (SDN_Types.flow*int) list SwitchMap.t

module type UPDATE_ARGS = sig
  val ctl : Controller.t
  val get_switches : unit -> SDN_Types.switchId list
  val get_order : unit -> LC.order
  val get_prev_order : unit -> LC.order
end

module type CONSISTENT_UPDATE_ARGS = sig
  val ctl : Controller.t
  val get_switches : unit -> SDN_Types.switchId list
  val get_order : unit -> LC.order
  val get_prev_order : unit -> LC.order
  val get_nib : unit -> Net.Topology.t
  val get_edge : unit -> edge
  val set_edge : edge -> unit
end

module type UPDATE = sig

  val bring_up_switch : SDN.switchId ->
    ?old:LC.t ->
    NetKAT_LocalCompiler.t ->
    unit Deferred.t

  val implement_policy :
    ?old:LC.t ->
    NetKAT_LocalCompiler.t ->
    unit Deferred.t

end

module BestEffort (Args : UPDATE_ARGS) : UPDATE = struct
  open Args

  let restrict sw_id repr =
    LC.restrict NetKAT_Types.(Switch sw_id) repr

  let install_flows_for sw_id table =
    let to_flow_mod p f = M.FlowModMsg (SDN_OpenFlow0x01.from_flow p f) in
    let priority = ref 65536 in
    Deferred.List.iter table ~f:(fun flow ->
      decr priority;
      Controller.send ctl sw_id (0l, to_flow_mod !priority flow)
      >>| function
        | `Drop exn -> raise exn
        | `Sent _   -> ())

  let delete_flows_for sw_id =
    let delete_flows = M.FlowModMsg OpenFlow0x01_Core.delete_all_flows in
    Controller.send ctl sw_id (5l, delete_flows)
    >>| function
      | `Drop exn -> raise exn
      | `Sent _   -> ()

  let bring_up_switch (sw_id : SDN.switchId) ?old new_r =
    match old with
    | Some(old_r) when
     (get_prev_order () = get_order ()  &&
        LC.equal (restrict sw_id old_r) (restrict sw_id new_r)) ->
      Log.debug ~tags
        "[policy] Skipping identical policy update for swithc %Lu" sw_id ;
      return ()
    | _ ->
      let table = LC.(to_table sw_id new_r) in
      Monitor.try_with ~name:"BestEffort.bring_up_switch" (fun () ->
        delete_flows_for sw_id >>= fun () ->
        install_flows_for sw_id table)
      >>= function
        | Ok x       -> return x
        | Error _exn ->
          Log.debug ~tags
            "switch %Lu: disconnected while attempting to bring up... skipping" sw_id;
          Log.flushed () >>| fun () ->
          Printf.eprintf "%s\n%!" (Exn.to_string _exn)

  let implement_policy ?old repr =
    Deferred.List.iter (get_switches ()) (fun sw_id ->
      bring_up_switch sw_id ?old repr)
end

module PerPacketConsistent (Args : CONSISTENT_UPDATE_ARGS) : UPDATE = struct
  open SDN_Types
  open Args

  let install_flows_for sw_id ?old table =
    let to_flow_mod p f = M.FlowModMsg (SDN_OpenFlow0x01.from_flow p f) in
    let priority = ref 65536 in
    Deferred.List.iter table ~f:(fun flow ->
      decr priority;
      Controller.send ctl sw_id (0l, to_flow_mod !priority flow)
      >>| function
        | `Drop exn -> raise exn
        | `Sent _   -> ())

  let delete_flows_for sw_id =
    let delete_flows = M.FlowModMsg OpenFlow0x01_Core.delete_all_flows in
    Controller.send ctl sw_id (5l, delete_flows)
    >>| function
      | `Drop exn -> raise exn
      | `Sent _   -> ()

  let specialize_action ver internal_ports actions =
    List.fold_right actions ~init:[] ~f:(fun action acc ->
      begin match action with
      | Output (Physical   pt) ->
        if not (Net.Topology.PortSet.mem internal_ports pt) then
          [Modify (SetVlan None)]
        else
          [Modify (SetVlan (Some ver))]
      | Output (Controller n ) ->
        [Modify (SetVlan None)]
      | Output _               -> assert false
      | _                      -> acc
      end @ (action :: acc))

  let specialize_edge_to (ver : int) internal_ports (table : SDN.flowTable) =
    let vlan_none = 65535 in
    List.filter_map table ~f:(fun flow ->
      begin match flow.pattern.Pattern.inPort with
      | Some pt when Net.Topology.PortSet.mem internal_ports pt ->
        None
      | _ ->
        Some { flow with
          pattern = { flow.pattern with Pattern.dlVlan = Some vlan_none }
        ; action  = List.map flow.action ~f:(fun x ->
            List.map x ~f:(specialize_action ver internal_ports))
        }
      end)

  let specialize_internal_to (ver : int) internal_ports (table : SDN.flowTable) =
    List.map table ~f:(fun flow ->
      { flow with
        pattern = { flow.pattern with Pattern.dlVlan = Some ver }
      ; action  = List.map flow.action ~f:(fun x ->
          List.map x ~f:(specialize_action ver internal_ports))
      })

  let clear_policy_for (ver : int) sw_id =
    let open OpenFlow0x01_Core in
    let clear_version_message = M.FlowModMsg { SDN_OpenFlow0x01.from_flow 0
      { pattern = { Pattern.match_all with Pattern.dlVlan = Some ver }
      ; action = []
      ; cookie = 0L
      ; idle_timeout = Permanent
      ; hard_timeout = Permanent
      } with command = DeleteFlow } in
    Monitor.try_with ~name:"PerPacketConsistent.clear_policy_for" (fun () ->
      Controller.send ctl sw_id (5l, clear_version_message))
    >>| function
      | Ok (`Sent _)    -> ()
      | Ok (`Drop _exn)
      | Error _exn      ->
        Log.error ~tags "switch %Lu: Failed to delete flows for ver %d" sw_id ver

  let internal_install_policy_for (ver : int) repr (sw_id : switchId) =
    begin let open Deferred.Result in
    Monitor.try_with ~name:"PerPacketConsistent.internal_install_policy_for" (fun () ->
      let table0 = LC.(to_table sw_id repr) in
      let table1 = specialize_internal_to
        ver (TUtil.internal_ports (get_nib ()) sw_id) table0 in
      assert (List.length table1 > 0);
      install_flows_for  sw_id table1)
    >>= fun () -> Controller.barrier ctl sw_id
    end
    >>| function
      | Ok () ->
        Log.debug ~tags
          "switch %Lu: installed internal table for ver %d" sw_id ver;
      | Error _ ->
        Log.debug ~tags
          "switch %Lu: disconnected while installing internal table for ver %d... skipping" sw_id ver

  (* Comparison should be made based on patterns only, not actions *)
  (* Assumes both FT are sorted in descending order by priority *)
  let rec flowtable_diff (ft1 : (SDN_Types.flow*int) list) (ft2 : (SDN_Types.flow*int) list) =
    let open SDN_Types in
    match ft1,ft2 with
    | (flow1,pri1)::ft1, (flow2,pri2)::ft2 ->
      if pri1 > pri2 then
        (flow1, pri1) :: flowtable_diff ft1 ((flow2,pri2)::ft2)
      else if pri1 = pri2 && flow1.pattern = flow2.pattern then
        flowtable_diff ft1 ((flow2,pri2)::ft2)
      else
        flowtable_diff ((flow1,pri1) :: ft1) ft2
    | _, [] -> ft1
    | [], _ -> []

  (* Assumptions:
     - switch respects priorities when deleting flows
  *)
  let swap_update_for sw_id c_id new_table : unit Deferred.t =
    let open OpenFlow0x01_Core in
    let max_priority = 65535 in
    let old_table = match SwitchMap.find (get_edge ()) sw_id with
      | Some ft -> ft
      | None -> [] in
    let (new_table, _) = List.fold new_table ~init:([], max_priority)
        ~f:(fun (acc,pri) x -> ((x,pri) :: acc, pri - 1)) in
    let new_table = List.rev new_table in
    let del_table = List.rev (flowtable_diff old_table new_table) in
    let to_flow_mod prio flow =
      M.FlowModMsg (SDN_OpenFlow0x01.from_flow prio flow) in
    let to_flow_del prio flow =
      M.FlowModMsg ({SDN_OpenFlow0x01.from_flow prio flow with command = DeleteStrictFlow}) in
    (* Install the new table *)
    Deferred.List.iter new_table ~f:(fun (flow, prio) ->
      send ctl c_id (0l, to_flow_mod prio flow))
    (* Delete the old table from the bottom up *)
    >>= fun () -> Deferred.List.iter del_table ~f:(fun (flow, prio) ->
      send ctl c_id (0l, to_flow_del prio flow))
    >>| fun () ->
    set_edge (SwitchMap.add (get_edge ()) sw_id new_table)

  let edge_install_policy_for ver repr (sw_id : switchId) : unit Deferred.t =
    begin let open Deferred.Result in
    Monitor.try_with ~name:"PerPacketConsistent.edge_install_policy_for" (fun () ->
      let table = LC.(to_table sw_id repr) in
      let edge_table = specialize_edge_to
        ver (TUtil.internal_ports (get_nib ()) sw_id) table in
      Log.debug ~tags
        "switch %Lu: Installing edge table for ver %d" sw_id ver;
      swap_update_for sw_id sw_id edge_table)
    >>= fun () -> Controller.barrier ctl sw_id
    end
    >>| function
      | Ok () ->
        Log.debug ~tags "switch %Lu: installed edge table for ver %d" sw_id ver
      | Error _ ->
        Log.debug ~tags "switch %Lu: disconnected while installing edge table for ver %d... skipping" sw_id ver

  let ver = ref 1

  let implement_policy ?old repr : unit Deferred.t =
    (* XXX(seliopou): It might be better to iterate over client ids rather than
     * switch ids. A client id is guaranteed to be unique within a run of a
     * program, whereas a switch id may be reused across client ids, i.e., a
     * switch connects, disconnects, and connects again. Due to this behavior,
     * it may be possible to get into an inconsistent state below. Maybe. *)
    let switches = get_switches () in
    let ver_num = !ver + 1 in
    (* Install internal update *)
    Log.debug ~tags "Installing internal tables for ver %d" ver_num;
    Log.flushed ()
    >>= fun () ->
    Deferred.List.iter switches (internal_install_policy_for ver_num repr)
    >>= fun () ->
    (Log.debug ~tags "Installing edge tables for ver %d" ver_num;
     Log.flushed ())
    >>= fun () ->
    (* Install edge update *)
    Deferred.List.iter switches (edge_install_policy_for ver_num repr)
    >>= fun () ->
    (* Delete old rules *)
    Deferred.List.iter switches (clear_policy_for (ver_num - 1))
    >>| fun () ->
      incr ver

  let bring_up_switch (sw_id : switchId) ?old repr =
    Monitor.try_with ~name:"PerPacketConsistent.bring_up_switch" (fun () ->
      delete_flows_for sw_id >>= fun () ->
      internal_install_policy_for !ver repr sw_id >>= fun () ->
      edge_install_policy_for !ver repr sw_id)
    >>= function
      | Ok x -> return ()
      | Error _exn ->
        Log.debug ~tags
          "switch %Lu: disconnected while attempting to bring up... skipping" sw_id;
        Log.flushed () >>| fun () ->
        Printf.eprintf "%s\n%!" (Exn.to_string _exn)

end
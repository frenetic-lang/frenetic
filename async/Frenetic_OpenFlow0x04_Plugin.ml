open Core.Std
open Async.Std
module Log = Frenetic_Log

(* Marshal and send a message to the switch *)
let send_message (to_client : Writer.t) (xid : Frenetic_OpenFlow_Header.xid)
  (message : Frenetic_OpenFlow0x04.Message.t) : unit =
  let raw_message = Frenetic_OpenFlow0x04.Message.marshal xid message in
  Writer.write to_client raw_message

(* Send group messages to switch to make group table *)
let implement_group_table (writer : Writer.t) (tbl : Frenetic_GroupTable0x04.t) : unit =
  let msgs = Frenetic_GroupTable0x04.commit tbl in
  let msg_num = List.length msgs in
  List.iteri msgs ~f:(fun i msg -> send_message writer (Int32.of_int_exn (9000 + i)) msg);
  if msg_num <> 0 then
    Log.info "Sent %d Group Table message(s)" (List.length msgs)

(* Add mask so that the meta value can be changed *)
let mask_meta (meta_id : int) =
  Frenetic_OpenFlow0x04.{ m_value = Int64.of_int meta_id; m_mask = Some 64L }

(* Send FlowMod messages to switch to implement policy *)
let implement_flow (writer : Writer.t) (fdd : Frenetic_NetKAT_Compiler.t)
  (layout : Frenetic_NetKAT_Compiler.flow_layout)
  (sw_id : Frenetic_OpenFlow.switchId) : unit =
  let open Frenetic_OpenFlow0x04 in
  let open Frenetic_NetKAT_Compiler in
  let (flow_rows, group_tbl) = to_multitable sw_id layout fdd in
  implement_group_table writer group_tbl;
  List.iteri flow_rows ~f:(fun i row ->
    let (tbl, m_id) = row.flowId in
    let xid = Int32.of_int_exn i in
    let prio = 1000 - i in
    (* do not include meta value for table start, else everything drops *)
    let pat = if m_id = 0 then (Oxm.from_of_pattern row.pattern)
      else (OxmMetadata (mask_meta m_id))::(Oxm.from_of_pattern row.pattern) in
    let insts = match row.instruction with
      | `Action action_group -> Instructions.from_of_group action_group
      | `GotoTable (goto_t, goto_m) ->
        [WriteMetadata (mask_meta goto_m); GotoTable goto_t]
    in
    let message = Message.FlowModMsg (add_flow ~tbl ~prio ~pat ~insts) in
    Log.info "Sending flow to switch %Ld\n\ttable:%d\n\tpriority:%d\n\tpattern:%s\n\tinstructions:%s"
      sw_id tbl prio (Oxm.match_to_string pat) (Instructions.to_string insts);
    send_message writer xid message)

(* Send FlowMod messages to switch to implement the policy, use topology to
 * generate fault tolerant group tables. *)
let implement_tolerant_flow (writer : Writer.t) (fdd : Frenetic_NetKAT_Compiler.t)
  (topo : Frenetic_NetKAT_Net.Net.Topology.t) (sw_id : Frenetic_OpenFlow.switchId)
  : unit =
  let open Frenetic_OpenFlow0x04 in
  let flowtable = Frenetic_NetKAT_Compiler.to_table sw_id fdd in
  List.iteri flowtable ~f:(fun i row ->
    let tbl = 1 in
    let xid= Int32.of_int_exn i in
    let prio = 1000 - i in
    let pat = Oxm.from_of_pattern row.pattern in
    let insts = Instructions.from_of_group row.action in
    let message = Message.FlowModMsg (add_flow ~tbl ~prio ~pat ~insts) in
    Log.info "Sending flow to switch %Ld\n\ttable:%d\n\tpriority:%d\n\tpattern:%s\n\tinstructions:%s"
      sw_id tbl prio (Oxm.match_to_string pat) (Instructions.to_string insts);
    send_message writer xid message)

(* Respond to message from switch *)
let process_message (xid : Frenetic_OpenFlow_Header.xid) (message : Frenetic_OpenFlow0x04.Message.t)
  (message_sender : (Frenetic_OpenFlow_Header.xid -> Frenetic_OpenFlow0x04.Message.t -> unit))
  (flow_sender : Frenetic_OpenFlow.switchId -> unit) : unit =
  let open Frenetic_OpenFlow0x04 in
  match message with
  | Message.EchoRequest bytes -> message_sender xid (Message.EchoReply bytes)
  | Message.Hello _           -> message_sender 10l Message.FeaturesRequest
  | Message.FeaturesReply fts -> flow_sender fts.datapath_id
  | Message.Error error       -> Log.error "%s" (Error.to_string error)
  | _                         -> Log.info "Unsupported message type"

(* Parse incoming client messages and respond. `Finished is sent if an
 * error occurs, otherwise `Repeat indefinitely. *)
let read_respond_loop (reader : Reader.t)
  (message_sender : (Frenetic_OpenFlow_Header.xid -> Frenetic_OpenFlow0x04.Message.t -> unit))
  (flow_sender : Frenetic_OpenFlow.switchId -> unit) ()
  : [ `Finished of unit | `Repeat of unit ] Deferred.t =
  let header_buf = Bytes.create Frenetic_OpenFlow_Header.size in
  Reader.really_read reader header_buf
  >>= function
  | `Eof _ ->
    Log.info "Connection closed reading header";
    return (`Finished ())
  | `Ok ->
    let header = Frenetic_OpenFlow_Header.parse (Cstruct.of_string header_buf) in
    let message_len = header.length - Frenetic_OpenFlow_Header.size in
    let message_buf = Bytes.create message_len in
    Reader.really_read reader message_buf
    >>= function
    | `Eof _ ->
      Log.info "Error reading client message";
      return (`Finished ())
    | `Ok ->
      let (xid, body) = Frenetic_OpenFlow0x04.Message.parse header message_buf in
      process_message xid body message_sender flow_sender;
      return (`Repeat ())

(* Send the initil handshake, loop on client response *)
let client_handler (reader : Reader.t)
  (message_sender : (Frenetic_OpenFlow_Header.xid -> Frenetic_OpenFlow0x04.Message.t -> unit))
  (flow_sender : Frenetic_OpenFlow.switchId -> unit) : unit Deferred.t =
  Log.info "Client connected";
  message_sender 0l (Frenetic_OpenFlow0x04.Message.Hello [VersionBitMap [0x04]]);
  Log.info "Sent Hello";
  Deferred.repeat_until_finished () (read_respond_loop reader message_sender flow_sender)

(* Implement multi-table policies. Extract the policy from a kat file,
 * run client handler for each connecting client *)
let main (of_port : int) (pol_file : string)
  (layout : Frenetic_NetKAT_Compiler.flow_layout) () : unit =
  let open Frenetic_NetKAT_Compiler in
  Log.info "Starting OpenFlow 1.3 controller";
  Log.info "Using flow tables: %s" (layout_to_string layout);
  let pol_str = In_channel.read_all pol_file in
  let pol = Frenetic_NetKAT_Parser.policy_of_string pol_str in
  let compiler_opts = {default_compiler_options with field_order = `Static (List.concat layout)} in
  let fdd = compile_local pol ~options:compiler_opts in
  let _ = Tcp.Server.create ~on_handler_error:`Raise (Tcp.on_port of_port)
    (fun _ reader writer ->
      let message_sender = send_message writer in
      let flow_sender = implement_flow writer fdd layout in
      client_handler reader message_sender flow_sender)
  in ()

(* Implement fault tolerant policies. Extract the policy and topology from
 * kat and dot files, run client_handler for each connecting client
 * TODO(mulias): This is a SHAM. Parsing the topology from a .dot file is not
 * yet implemented. *)
let fault_tolerant_main (of_port : int) (pol_file : string)
  (topo_file : string) () : unit =
  Log.info "Starting OpenFlow 1.3 fault tolerant controller";
  let pol_str = In_channel.read_all pol_file in
  let pol = Frenetic_NetKAT_Parser.policy_of_string pol_str in
  let fdd = Frenetic_NetKAT_Compiler.compile_local pol in
  let topo = Frenetic_NetKAT_Net.Net.Topology.empty () in
  (* let topo = Frenetic_NetKAT_Net.Net.Parse.from_dotfile topo_file in *)
  let _ = Tcp.Server.create ~on_handler_error:`Raise (Tcp.on_port of_port)
    (fun _ reader writer ->
      let message_sender = send_message writer in
      let flow_sender = implement_tolerant_flow writer fdd topo in
      client_handler reader message_sender flow_sender)
  in ()

open Core.Std
open Async.Std

module S = SDN_Types
module Header = OpenFlow_Header
module OF1 = OpenFlow0x01
module OF1_Core = OpenFlow0x01_Core
module M1 = OF1.Message
module OF4 = OpenFlow0x04
module OF4_Core = OpenFlow0x04_Core
module M4 = OpenFlow0x04.Message

module Log = Async_OpenFlow_Log

module Platform = Async_OpenFlowChunk.Controller

module Clients = Hashtbl.Make(Platform.Client_id)

module Switches = Hashtbl.Make(Int64)

exception Handshake of Platform.Client_id.t * string

(* Use this as the ~tags argument to Log.info, Log.debug, etc. *)
let tags = [("openflow", "platform")]

(* helper functions to pattern match on OpenFlow type_code *)
type message_code = 
  | FeaturesReply
  | MultipartReply
  | Other

let int_to_message_code n = 
  let open Header in 
  if n = type_code_features_reply then FeaturesReply
  else if n = OF4.msg_code_to_int OF4.MULTIPART_RESP then MultipartReply
  else Other

(* constants *)
let max_version = 0x04

(* handshake protocol state *)
type handshake_state = 
  | SentFeaturesRequest0x01
  | SentFeaturesRequest0x04 
  | SentPortDescriptionRequest0x04 of S.switchId sexp_opaque
  | Connected0x01 of S.switchId sexp_opaque
  | Connected0x04 of S.switchId sexp_opaque
  with sexp 

type t = {
  sub : Platform.t;
  clients : handshake_state Clients.t;
  switches : Platform.Client_id.t Switches.t
}

(* helpers to construct OpenFlow messages *)
let features_request_msg version : Platform.m = 
  let open Header in 
  ({ version = version;
    type_code = type_code_features_request;
    length = size;
    xid = 0l; }, 
   Cstruct.of_string "")

let port_description_request_msg : Platform.m =
  let open Header in 
  let open OF4 in 
  let mpr = (0l, Message.MultipartReq (OF4_Core.PortsDescReq)) in
  Async_OpenFlow0x04.Message.marshal' mpr
  
let handshake_error (c_id:Platform.Client_id.t) (str:string) : 'a = 
  raise (Handshake (c_id,str))

let send t c_id msg = 
  Platform.send t.sub c_id msg >>| function
    | `Sent _ -> []
    | `Drop exn -> raise exn
        
let features t evt =
  let open Header in 
  match evt with 
  | `Connect (c_id, version) ->
    Log.info ~tags "Sent Hello";
    let next_handshake_state =
       match version with
       | 0x01 -> SentFeaturesRequest0x01
       | 0x04 -> SentFeaturesRequest0x04
       | _ ->
          handshake_error c_id
            (Printf.sprintf "unexpected version: %d%!" version) in
     Clients.replace t.clients c_id next_handshake_state;
     send t c_id (features_request_msg version)
  | `Disconnect (c_id,_) -> 
     Log.info ~tags "Client disconnected";
     begin match Clients.find t.clients c_id with
       | Some (Connected0x01 switch_id)
       | Some (Connected0x04 switch_id) ->
          ignore (Switches.remove t.switches switch_id);
       | _ -> ()
     end;
     ignore (Clients.remove t.clients c_id);
     return []
  | `Message (c_id,(hdr,bits)) -> 
     let of_type_code = int_to_message_code hdr.type_code in 
     let handshake_state = Clients.find t.clients c_id in
     begin 
       match of_type_code, handshake_state with
       | FeaturesReply, Some SentFeaturesRequest0x01 -> 
          Log.info ~tags "SentFeaturesRequest0x01";
          begin 
            match M1.parse hdr (Cstruct.to_string bits) with 
            | (_, M1.SwitchFeaturesReply feats) -> 
               let get_port pd = VInt.Int16 pd.OF1.PortDescription.port_no in 
               let switch_id = feats.OF1.SwitchFeatures.switch_id in
               let switch_ports = List.map feats.OF1.SwitchFeatures.ports ~f:get_port in 
               let feats = { S.switch_id = switch_id;
                             S.switch_ports = switch_ports } in 
               Clients.replace t.clients c_id (Connected0x01 switch_id);
               ignore (Switches.add t.switches switch_id c_id);
               return [feats]
            | _ -> 
               handshake_error c_id 
                 (Printf.sprintf "expected features reply in %s%!" (to_string hdr))
          end
     | FeaturesReply, Some SentFeaturesRequest0x04 ->
        Log.info ~tags "SentFeaturesRequest0x04";
        begin 
          match M4.parse hdr (Cstruct.to_string bits) with
          | (_, M4.FeaturesReply feats) -> 
             let switch_id = feats.OF4.SwitchFeatures.datapath_id in
             Clients.replace t.clients c_id (SentPortDescriptionRequest0x04 switch_id);
             send t c_id port_description_request_msg
          | _ -> 
             handshake_error c_id 
               (Printf.sprintf "expected features reply in %s%!" (to_string hdr))
        end
     | MultipartReply, Some (SentPortDescriptionRequest0x04 switch_id) -> 
        Log.info ~tags "SentPortDescriptionRequest0x04";
        begin
          match M4.parse hdr (Cstruct.to_string bits) with 
          | (_, M4.MultipartReply (OF4_Core.PortsDescReply ports)) -> 
             let get_port pd = VInt.Int32 pd.OF4_Core.port_no in 
             let switch_ports = List.map ports ~f:get_port in 
             let feats = { S.switch_id = switch_id;
                           S.switch_ports = switch_ports } in 
             Clients.replace t.clients c_id (Connected0x04 switch_id);
             ignore (Switches.add t.switches switch_id c_id);
             return [feats]
          | _ -> 
             handshake_error c_id 
               (Printf.sprintf "expected port description reply in %s%!" (to_string hdr))
        end
     | _, Some (Connected0x01 sw_id) -> 
        let _, msg = M1.parse hdr (Cstruct.to_string bits) in
        Log.error ~tags "received unhandled message %s" (M1.to_string msg);
        return []
     | _, Some (Connected0x04 sw_id) -> 
        let _, msg = M4.parse hdr (Cstruct.to_string bits) in
        Log.error ~tags "received unhandled message %s" (M4.to_string msg);
        return []
     | _, Some state -> 
        return []
     | _, None -> 
        Log.error ~tags "got %s in unknown state" (Header.to_string hdr);
        return []
     end

let create ?max_pending_connections
    ?verbose
    ?log_disconnects
    ?buffer_age_limit ~port () =
  Platform.create ?max_pending_connections ?verbose ?log_disconnects
    ?buffer_age_limit ~port ()
  >>| function t ->
      Log.info ~tags "accepting switches on port %d" port;
      { sub = t
      ; clients = Clients.create ()
      ; switches = Switches.create ()
      }

let accept_switches (t : t) =
  let open Async_OpenFlow_Platform.Trans in
  let open Platform in
  let stages =
    (local (fun t -> t.sub)
      (handshake max_version >=> echo))
    >=> features in
  run stages t (listen t.sub)

let send_msg0x01 t c_id msg =
  let msg_c = Async_OpenFlow0x01.Message.marshal' (0l, msg) in
  Deferred.ignore (send t c_id msg_c)

let send_msg0x04 t c_id msg =
  Log.info ~tags "send_msg0x04 %s" (OF4.Message.to_string msg);
  let msg_c = Async_OpenFlow0x04.Message.marshal' (0l, msg) in
  Deferred.ignore (send t c_id msg_c)

let setup_flow_table t (sw:S.switchId) (tbl:S.flowTable) =
  Log.info ~tags "setup_flow_table";
  let c_id = match Switches.find t.switches sw with
    | Some c_id -> 
       c_id
    | None -> 
       failwith "No such switch" in 
  match Clients.find t.clients c_id with
  | Some Connected0x01 _ -> 
     let priority = ref 65536 in
     let send_flow_mod (fl : S.flow) : unit Deferred.t =
       decr priority;
       send_msg0x01 t c_id (M1.FlowModMsg (SDN_OpenFlow0x01.from_flow !priority fl)) in
     let delete_flows = send_msg0x01 t c_id (M1.FlowModMsg OF1_Core.delete_all_flows) in
     let flow_mods = List.map tbl ~f:send_flow_mod in
     delete_flows >>= fun _ -> 
     Deferred.all_ignore flow_mods
  | Some Connected0x04 _ -> 
     let tbl = SDN_OpenFlow0x04.fix_vlan_in_table tbl in 
     let priority = ref 65535 in
     let group_table = GroupTable0x04.create () in 
     let mk_flow_mod (fl : S.flow) =
      let flow_mod = SDN_OpenFlow0x04.from_flow group_table !priority fl in
       decr priority;
       M4.FlowModMsg flow_mod in
     let delete_flows = send_msg0x04 t c_id (M4.FlowModMsg OF4_Core.delete_all_flows) in
     let delete_groups = send_msg0x04 t c_id (M4.GroupModMsg (OF4_Core.DeleteGroup (OF4_Core.All, 0xfffffffcl) )) in
     let flow_mods = List.map tbl ~f:mk_flow_mod in
     let group_mods = GroupTable0x04.commit group_table in
     delete_flows >>= fun _ -> 
     delete_groups >>= fun _ -> 
     Deferred.List.iter group_mods ~f:(send_msg0x04 t c_id) >>= fun _ -> 
     Deferred.List.iter flow_mods ~f:(send_msg0x04 t c_id)
  | _ -> 
     failwith "Switch not connected"

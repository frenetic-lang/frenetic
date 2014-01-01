open Core.Std
open Async.Std

module S = SDN_Types
module C = Async_OpenFlowChunk.Controller
module H = OpenFlow_Header
module OF1 = OpenFlow0x01
module M1 = OF1.Message
module OF4 = OpenFlow0x04
module OF4_Core = OpenFlow0x04_Core
module M4 = OpenFlow0x04.Message

type switchId = S.switchId

type flowTable = S.flowTable

type switchFeatures = S.switchFeatures

module Clients = Hashtbl.Make(C.Client_id)

module Switches = Hashtbl.Make(VInt)

let max_version = 0x04

type handshake_state = 
  | SentHello
  | SentFeaturesRequest0x01
  | SentFeaturesRequest0x04 
  | SentPortDescriptionRequest0x04 of VInt.t
  | Connected0x01 of VInt.t
  | Connected0x04 of VInt.t

type message_code = 
  | Hello
  | EchoRequest
  | FeaturesReply
  | MultipartReply
  | Other

let int_to_message_code n = 
  let open H in 
  if n = type_code_echo_request then EchoRequest
  else if n = type_code_hello then Hello
  else if n = type_code_features_reply then FeaturesReply
  else if n = OF4.msg_code_to_int OF4.MULTIPART_REQ then MultipartReply
  else Other

(* global state *)
let clients = Clients.create ()

let switches = Switches.create ()

(* helpers to construct messages *)
let hello_msg version =  
  let open H in 
  ({ version = version; 
    type_code = type_code_hello;
    length = size; 
    xid = 0l; },
   Cstruct.of_string "")

let features_request_msg version = 
  let open H in 
  ({ version = version;
    type_code = type_code_features_request;
    length = size;
    xid = 0l; }, 
   Cstruct.of_string "")

let echo_reply_msg version = 
  let open H in 
  ({ version = version;
    type_code = type_code_echo_reply;
    length = size;
    xid = 0l; }, 
   Cstruct.of_string "")

let port_description_request_msg version = 
  let open H in 
  let open OF4 in 
  let mpr = OF4_Core.PortsDescReq in 
  let bits = Cstruct.create (MultipartReq.sizeof mpr) in 
  ignore (MultipartReq.marshal bits mpr);
  ({ version = 0x04;
    type_code = msg_code_to_int MULTIPART_REQ;
    length = size;
    xid = 0l; }, 
   bits)
  
let handshake_error c_id str = 
  raise (C.Handshake (c_id,str))
    
let handshake t evt = match evt with 
  | `Connect c_id -> 
     ignore (Clients.add clients c_id SentHello);
     C.send t c_id (hello_msg max_version)
     >>| C.ensure
  | `Disconnect (c_id,_) -> 
     (match Clients.find clients c_id with 
     | Some (Connected0x01 switch_id) -> 
        ignore (Clients.remove clients c_id);
        ignore (Switches.remove switches switch_id);
        return None
     | Some _ ->  
        ignore (Clients.remove clients c_id);
        return None
     | None -> 
        return None)
  | `Message (c_id,(hdr,bits)) -> 
     match int_to_message_code hdr.H.type_code, Clients.find clients c_id with 
     | EchoRequest, Some _ -> 
        C.send t c_id (echo_reply_msg hdr.H.version)
        >>| C.ensure
     | Hello, Some SentHello ->           
        let version = min hdr.H.version max_version in 
        let next = match version with 
          | 0x01 -> SentFeaturesRequest0x01
          | 0x04 -> SentFeaturesRequest0x04
          | _ -> 
             handshake_error c_id 
               (Printf.sprintf "unexpected version %d in header: %s%!" version (H.to_string hdr)) in 
        ignore (Clients.add clients c_id next);
        C.send t c_id (features_request_msg version)
        >>| C.ensure
     | FeaturesReply, Some SentFeaturesRequest0x01 -> 
        (match M1.parse hdr (Cstruct.to_string bits) with 
         | (_, M1.SwitchFeaturesReply feats) -> 
            let get_port pd = VInt.Int16 pd.OF1.PortDescription.port_no in 
            let switch_id = VInt.Int64 feats.OF1.SwitchFeatures.switch_id in 
            let switch_ports = List.map feats.OF1.SwitchFeatures.ports ~f:get_port in 
            let feats = { S.switch_id = switch_id;
                          S.switch_ports = switch_ports } in 
            ignore (Clients.add clients c_id (Connected0x01 switch_id));
            ignore (Switches.add switches switch_id c_id);
            return (Some feats)
         | _ -> 
            handshake_error c_id 
              (Printf.sprintf "expected features reply in %s%!" (H.to_string hdr)))
     | FeaturesReply, Some SentFeaturesRequest0x04 -> 
        (match M4.parse hdr (Cstruct.to_string bits) with
         | (_, M4.FeaturesReply feats) -> 
            let switch_id = VInt.Int64 feats.OF4.SwitchFeatures.datapath_id in 
            ignore (Clients.add clients c_id (SentPortDescriptionRequest0x04 switch_id));
            C.send t c_id (port_description_request_msg hdr.H.version)
            >>| C.ensure
         | _ -> 
            handshake_error c_id 
              (Printf.sprintf "expected features reply in %s%!" (H.to_string hdr)))
     | MultipartReply, Some (SentPortDescriptionRequest0x04 switch_id) -> 
        (match M4.parse hdr (Cstruct.to_string bits) with 
         | (_, M4.MultipartReply (OF4_Core.PortsDescReply ports)) -> 
            let get_port pd = VInt.Int32 pd.OF4_Core.port_no in 
            let switch_ports = List.map ports ~f:get_port in 
            let feats = { S.switch_id = switch_id;
                          S.switch_ports = switch_ports } in 
            ignore (Clients.add clients c_id (Connected0x04 switch_id));
            ignore (Switches.add switches switch_id c_id);
            return (Some feats)
         | _ -> 
            handshake_error c_id 
              (Printf.sprintf "expected port description reply in %s%!" (H.to_string hdr)))
     | _ -> 
        return None

let accept_switches port = 
  C.create port >>= fun t -> 
  return (Pipe.filter_map' (C.listen t) ~f:(handshake t))

let setup_flow_table = failwith "not yet implemented"

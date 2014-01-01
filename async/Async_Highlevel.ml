open Core.Std
open Async.Std

module S = SDN_Types
module C = Async_OpenFlowChunk.Controller
module H = OpenFlow_Header
module OF1 = OpenFlow0x01
module M1 = OF1.Message
module OF4 = OpenFlow0x04
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
  | SentPortDescriptionRequest0x04 
  | Connected0x01
  | Connected0x04

type message_code = 
  | Hello
  | EchoRequest
  | FeaturesReply
  | Other

let int_to_message_code n = 
  let open H in 
  if n = type_code_echo_request then EchoRequest
  else if n = type_code_hello then Hello
  else if n = type_code_features_reply then FeaturesReply
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

let handshake_error c_id str = 
  raise (C.Handshake (c_id,str))
    
let handshake t evt = match evt with 
  | `Connect c_id -> 
     ignore (Clients.add clients c_id SentHello);
     C.send t c_id (hello_msg max_version)
     >>| C.ensure
  | `Disconnect (c_id,_) -> 
     ignore (Clients.remove clients c_id);
     return None
  | `Message (c_id,(hdr,bits)) -> 
     match int_to_message_code hdr.H.type_code, Clients.find clients c_id with 
     | EchoRequest, Some _ -> 
        C.send t c_id (echo_reply_msg hdr.H.version)
        >>| C.ensure
     | Hello, Some SentHello ->           
        let version = min hdr.H.version max_version in 
        let next = 
          match version with 
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
            let open OF1.SwitchFeatures in 
            let open OF1.PortDescription in 
            let switch_id = VInt.Int64 feats.switch_id in 
            let switch_ports = List.map feats.ports (fun pd -> VInt.Int16 pd.port_no) in 
            let feats = { S.switch_id = switch_id;
                          S.switch_ports = switch_ports } in 
            return (Some feats)
         | _ -> 
            handshake_error c_id 
              (Printf.sprintf "expected features reply in %s%!" (H.to_string hdr)))
     | FeaturesReply, Some SentFeaturesRequest0x04 -> 
        failwith "Not yet implemented"
     | _ -> 
        failwith "Unexpected message"
                  
let accept_switches port = 
  let open Async_OpenFlow_Platform.Trans in 
  C.create port >>= fun t -> 
  return (Pipe.filter_map' (C.listen t) ~f:(handshake t))

let setup_flow_table = failwith "not yet implemented"

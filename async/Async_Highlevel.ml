open Core.Std
open Async.Std

module S = SDN_Types
module C = Async_OpenFlowChunk.Controller
module P = Async_OpenFlow_Platform
module H = OpenFlow_Header
module OF1 = OpenFlow0x01
module M1 = OF1.Message
module OF4 = OpenFlow0x04
module M4 = OpenFlow0x04.Message

type switchId = S.switchId

type flowTable = S.flowTable

type switchFeatures = S.switchFeatures

module ClientMap = Map.Make(C.Client_id)

type t = 
  { chunk : C.t;
    mutable clients : int ClientMap.t }

let max_version = 0x04

let hello_msg =  
  let open H in 
  ({ version = max_version; 
    type_code = type_code_hello;
    length = size; 
    xid = 0l; },
   Cstruct.of_string "")

let features_request_msg v = 
  let open H in 
  ({ version = v;
    type_code = type_code_features_request;
    length = size;
    xid = 0l; }, 
   Cstruct.of_string "")

let echo_reply_msg v = 
  let open H in 
  ({ version = v;
    type_code = type_code_echo_reply;
    length = size;
    xid = 0l; }, 
   Cstruct.of_string "")

let wrap f evt = 
  f ();
  evt
    
let handshake1 t evt =
  match evt with
  | `Connect c_id ->
     C.send t.chunk c_id hello_msg
     >>| C.ensure
     >>| wrap (fun () -> t.clients <- ClientMap.add t.clients c_id max_version)
  | _ -> return (Some evt)

let handshake2 t evt = 
  match evt with 
  | `Message (c_id, (hdr,_)) 
       when ClientMap.mem t.clients c_id ->
     if hdr.H.type_code = H.type_code_hello then 
       let v = min hdr.H.version (ClientMap.find_exn t.clients c_id) in 
       t.clients <- ClientMap.add t.clients c_id v;
       C.send t.chunk c_id (features_request_msg v)
       >>| C.ensure
     else
       raise (C.Handshake (c_id, Printf.sprintf 
                                   "Expected 0 code in header :%s%!" 
                                   (H.to_string hdr)))
  | `Disconnect(c_id,_) when ClientMap.mem t.clients c_id -> 
     t.clients <- ClientMap.remove t.clients c_id;
     return None
  | _ -> 
     return (Some evt)

let handshake3 t evt = 
  match evt with 
  | `Message(c_id, (hdr,bits)) 
       when ClientMap.mem t.clients c_id && 
            hdr.H.type_code = H.type_code_features_reply -> 
     let s_id, pts = 
       match ClientMap.find_exn t.clients c_id with 
       | 0x01 -> 
          (match M1.parse hdr (Cstruct.to_string bits) with 
           | (_, M1.SwitchFeaturesReply feats) -> 
              let open OF1.SwitchFeatures in 
              let open OF1.PortDescription in 
              (VInt.Int64 feats.switch_id, 
               List.map feats.ports (fun pd -> VInt.Int16 pd.port_no))
           | _ -> 
              raise (C.Handshake (c_id, 
                                  Printf.sprintf
                                    "Expected features reply in header : %s%!"
                                    (H.to_string hdr))))
       | 0x04 -> 
          raise (C.Handshake (c_id, Printf.sprintf
                                      "Not yet implemented : %s%!"
                                      (H.to_string hdr)))
       | v -> raise (C.Handshake (c_id, Printf.sprintf 
                                          "Unexpected version %d in header : %s%!" 
                                          v 
                                          (H.to_string hdr))) in 
                  
     let feats = 
       let open SDN_Types in 
       { switch_id = s_id; 
         switch_ports = pts } in 
     return (Some (`Features feats))
  | _ -> 
     return (Some evt)
                                            
let echo t evt = 
  match evt with 
  | `Message (c_id, (hdr, bits)) 
       when ClientMap.mem t.clients c_id && 
            hdr.H.type_code = H.type_code_echo_request -> 
     (* JNF: dumb, slow protocol typechecking. 
             could just grab hdr.H.version *)
     let v = ClientMap.find_exn t.clients c_id in 
     C.send t.chunk c_id (echo_reply_msg v)
    >>| C.ensure
  | _ -> 
     return (Some evt)

let filter_feats t evt = 
  match evt with 
  | `Features f -> 
     return (Some f)
  | _ -> 
     return None

let accept_switches port = 
  let open Async_OpenFlow_Platform.Trans in 
  C.create port >>= fun t -> 
  let t = 
    { chunk = t;
      clients = ClientMap.empty } in 
  let stages = 
    handshake1 >=>
    handshake2 >=>
    handshake3 >=>
    echo >=>
    filter_feats in 
  return (run stages t (C.listen t.chunk))

let setup_flow_table = failwith "not yet implemented"

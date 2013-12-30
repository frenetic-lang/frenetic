open Core.Std
open Async.Std

module Platform = Async_OpenFlow_Platform
module Header = OpenFlow_Header
module M = OpenFlow0x01.Message

module Message : Platform.Message with type t = (OpenFlow0x01.xid * M.t) = struct

  type t = (OpenFlow0x01.xid * M.t) sexp_opaque with sexp

  let header_of (xid, m) = M.header_of xid m
  let parse hdr buf = M.parse hdr (Cstruct.to_string buf)
  let marshal (xid, m) buf = M.marshal_body m buf
  let to_string (_, m) = M.to_string m

  let marshal' msg =
    let hdr = header_of msg in
    let body_len = hdr.Header.length - Header.size in
    let body_buf = Cstruct.create body_len in
    marshal msg body_buf;
    (hdr, body_buf)

end

include Async_OpenFlow_Message.MakeSerializers (Message)

module Controller = struct
  module Platform = Platform.Make(Message)
  module Switch_id = Platform.Switch_id

  module SwitchTable = Map.Make(Switch_id)

  type h =
    | AwaitHello
    | AwaitFeatures

  exception Handshake of Switch_id.t * string

  type t = {
    platform : Platform.t;
    mutable switches : OpenFlow0x01.SwitchFeatures.t SwitchTable.t;
    mutable handshakes : h SwitchTable.t
  }

  let init_handshake t (c_id : Switch_id.t)  =
    let open OpenFlow0x01.Message in
    Platform.send t.platform c_id (0l, Hello (Cstruct.of_string ""))
    >>= function
      | `Sent _ ->
        t.handshakes <- SwitchTable.add t.handshakes c_id AwaitHello;
        return None
      | `Drop exn -> raise exn

  let handshake t (c_id : Switch_id.t) (h : h) (msg : Platform.m) =
    let open OpenFlow0x01.Message in
    match h, msg with
      | AwaitHello, (_, Hello _) ->
        Platform.send t.platform c_id (0l, SwitchFeaturesRequest)
        >>= (function
          | `Sent _ ->
            t.handshakes <- SwitchTable.add t.handshakes c_id AwaitFeatures;
            return None
          | `Drop exn ->
            t.handshakes <- SwitchTable.remove t.handshakes c_id;
            raise exn)
      | AwaitFeatures, (_, SwitchFeaturesReply feats) ->
        t.switches <- SwitchTable.add t.switches c_id feats;
        t.handshakes <- SwitchTable.remove t.handshakes c_id;
        return (Some(`Connect c_id))
      | _, _ -> raise (Handshake (c_id, "unknown handshake state"))

  let handshake (t : t) msg =
    match msg with
      | `Connect s_id -> init_handshake t s_id
      | `Disconnect (s_id, e) ->
        t.switches <- SwitchTable.remove t.switches s_id;
        return (Some(`Disconnect(s_id, e)))
      | `Message (s_id, msg') ->
        begin match SwitchTable.find t.switches s_id with
          | Some _ -> return (Some(`Message(s_id, msg')))
          | None ->
            begin match SwitchTable.find t.handshakes s_id with
              | Some h -> handshake t s_id h msg'
              | None -> raise (Handshake (s_id, "received message from unknown switch"))
            end
        end

  let echo (t : t) evt =
    let open OpenFlow0x01.Message in
    match evt with
      | `Message (s_id, (t_id, EchoRequest bytes)) ->
        Platform.send t.platform s_id (t_id, EchoReply bytes) >>| (fun _ -> None)
      | _ -> return (Some(evt))

  let create ?max_pending_connections ?verbose ?log_disconnects ?buffer_age_limit ~port =
    Platform.create ?max_pending_connections ?verbose ?log_disconnects
      ?buffer_age_limit ~port
    >>| function t -> {
      platform = t;
      switches = SwitchTable.empty;
      handshakes = SwitchTable.empty
    }

  let listen t =
    let open Async_OpenFlow_Platform.Trans in
    run (handshake >=> echo) t (Platform.listen t.platform)

  let close t = Platform.close t.platform
  let has_switch_id t s_id = SwitchTable.find t.switches s_id
  let send t = Platform.send t.platform
  let send_to_all t = Platform.send_to_all t.platform
  let client_addr_port t = Platform.client_addr_port t.platform
  let listening_port t = Platform.listening_port t.platform
end

open Core.Std

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
  open Async.Std

  module ChunkController = Async_OpenFlowChunk.Controller
  module Client_id = ChunkController.Client_id

  module SwitchSet = Set.Make(Client_id)

  type m = Message.t
  type t = {
    sub : ChunkController.t;
    mutable feat : SwitchSet.t;
  }

  type e = [
    | `Connect of Client_id.t
    | `Disconnect of Client_id.t * Sexp.t
    | `Message of Client_id.t * m
  ]

  type f = [
    | `Connect of Client_id.t * OpenFlow0x01.SwitchFeatures.t
    | `Disconnect of Client_id.t * Sexp.t
    | `Message of Client_id.t * m
  ]

  let create ?max_pending_connections
      ?verbose
      ?log_disconnects
      ?buffer_age_limit ~port () =
    ChunkController.create ?max_pending_connections ?verbose ?log_disconnects
      ?buffer_age_limit ~port ()
    >>| function t -> { sub = t; feat = SwitchSet.empty }

  let close t = ChunkController.close t.sub
  let has_switch_id t = ChunkController.has_switch_id t.sub
  let send t s_id msg = ChunkController.send t.sub s_id (Message.marshal' msg)
  let send_to_all t msg = ChunkController.send_to_all t.sub (Message.marshal' msg)
  let client_addr_port t = ChunkController.client_addr_port t.sub
  let listening_port t = ChunkController.listening_port t.sub

  let openflow0x01 t evt =
    match evt with
      | `Connect (c_id, version) ->
        if version = 0x01 then
          return (Some(`Connect c_id))
        else begin
          close t c_id;
          raise (ChunkController.Handshake (c_id, Printf.sprintf
                    "Negotiated switch version mismatch: expected %d but got %d%!"
                    0x01 version))
        end
      | `Message (c_id, (hdr, bits)) ->
         return (Some(`Message (c_id, Message.parse hdr bits)))
      | `Disconnect e -> return (Some(`Disconnect e))

  let features t evt =
    match evt with
      | `Connect (c_id) ->
        t.feat <- SwitchSet.add t.feat c_id;
        send t c_id (0l, M.SwitchFeaturesRequest) >>| ChunkController.ensure
      | `Message (c_id, (_, msg)) when SwitchSet.mem t.feat c_id ->
        t.feat <- SwitchSet.remove t.feat c_id;
        begin match msg with
          | M.SwitchFeaturesReply fs -> return(Some(`Connect(c_id, fs)))
          | _ ->
            close t c_id;
            raise (ChunkController.Handshake (c_id,
                    "Expected FEATURES_REPLY but received something else"))
        end
      | `Message (c_id, msg) -> return(Some(`Message(c_id, msg)))
      | `Disconnect (c_id, exn) ->
        t.feat <- SwitchSet.remove t.feat c_id;
        return (Some(`Disconnect(c_id, exn)))

  let listen t =
    let open Async_OpenFlow_Platform.Trans in
    let open ChunkController in
    let stages =
      (local (fun t -> t.sub)
        (handshake 0x01 >=> echo))
      >=> openflow0x01 in
    run stages t (listen t.sub)

end

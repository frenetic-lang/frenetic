open Core.Std

module Platform = Async_OpenFlow_Platform
module Header = OpenFlow_Header

module Message : Platform.Message 
  with type t = (Header.t * Cstruct.t) = struct

  type t = (Header.t * Cstruct.t) sexp_opaque with sexp

  let header_of (hdr, _) = hdr

  let parse hdr buf = (hdr, Cstruct.set_len buf (hdr.Header.length - Header.size))

  let marshal (hdr, body) buf =
    Cstruct.blit body 0 buf 0 (hdr.Header.length - Header.size)

  let marshal' x = x

  let to_string x = Sexp.to_string_hum (sexp_of_t x)

end

module Controller = struct
  open Async.Std

  module Platform = Platform.Make(Message)
  module Client_id = Platform.Client_id

  module SwitchSet = Set.Make(Client_id)

  exception Handshake of Client_id.t * string

  type m = Platform.m
  type t = {
    platform : Platform.t;
    mutable handshakes : SwitchSet.t
  }

  type e = Platform.e

  let ensure response =
    match response with
      | `Sent _ -> None
      | `Drop exn -> raise exn

  let handshake v t evt =
    let open Header in
    match evt with
      | `Connect s_id ->
        let header = { version = v; type_code = type_code_hello;
                       length = size; xid = 0l; } in
        Platform.send t.platform s_id (header, Cstruct.of_string "")
        >>| ensure
        >>| (fun e -> t.handshakes <- SwitchSet.add t.handshakes s_id; e)
      | `Message (s_id, msg) when SwitchSet.mem t.handshakes s_id ->
        let hdr, bits = msg in
        begin
          t.handshakes <- SwitchSet.remove t.handshakes s_id;

          if not (hdr.type_code = type_code_hello) then begin
            Platform.close t.platform s_id;
            raise (Handshake (s_id, Printf.sprintf
                      "Expected 0 code in header: %s%!"
                      (Header.to_string hdr)))
          end
        end;
        return (Some(`Connect (s_id, min hdr.version v)))
      | `Message x -> return(Some(`Message x))
      | `Disconnect (s_id, _) when SwitchSet.mem t.handshakes s_id ->
        t.handshakes <- SwitchSet.remove t.handshakes s_id;
        return None
      | `Disconnect x -> return(Some(`Disconnect x))

  let echo t evt =
    let open Header in
    match evt with
      | `Message (s_id, (hdr, bytes))
          when hdr.Header.type_code = type_code_echo_request ->
        Platform.send t.platform s_id ({ hdr with type_code = type_code_echo_reply }, bytes)
        >>| ensure
      | _ -> return (Some(evt))

  let create ?max_pending_connections ?verbose ?log_disconnects ?buffer_age_limit ~port =
    Platform.create ?max_pending_connections ?verbose ?log_disconnects
      ?buffer_age_limit ~port
    >>| function t -> {
      platform = t;
      handshakes = SwitchSet.empty
    }

  let listen t = Platform.listen t.platform

  let close t = Platform.close t.platform
  let has_switch_id t = Platform.has_switch_id t.platform
  let send t = Platform.send t.platform
  let send_to_all t = Platform.send_to_all t.platform
  let client_addr_port t = Platform.client_addr_port t.platform
  let listening_port t = Platform.listening_port t.platform
end

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

  module ClientTbl = Hashtbl.Make(Client_id)

  exception Handshake of Client_id.t * string

  module Conn = struct
    type t = {
      state : [ `Active | `Idle | `Kill ];
      version : int option;
      state_entered : Time.t;
      last_activity : Time.t
    }

    let create () : t =
      let now = Time.now () in
      { state = `Active
      ; version = None
      ; state_entered = now
      ; last_activity = now
      }

    let activity (t:t) : t =
      let t = { t with last_activity = Time.now () } in
      if t.state = `Idle then
        { t with state = `Active; state_entered = t.last_activity }
      else
        t

    let complete_handshake (t:t) (version:int) : t =
      activity { t with version = Some(version) }

    let idle (t:t) (span : Time.Span.t) : t * bool =
      let right_now = Time.now () in
      if t.state = `Active && Time.(add t.last_activity span <= right_now) then
        { t with state = `Idle; state_entered = right_now }, true
      else
        t, false

    let kill (t:t) (span : Time.Span.t) : t * bool =
      let right_now = Time.now () in
      if t.state = `Idle && Time.(add t.state_entered span <= right_now) then
        { t with state = `Kill; state_entered = right_now }, true
      else
        t, false

  end

  type t = {
    platform : Platform.t;
    clients  : Conn.t ClientTbl.t;
    mutable monitor_interval : Time.Span.t;
    mutable idle_wait : Time.Span.t;
    mutable kill_wait : Time.Span.t;
  }

  type m = Platform.m
  type e = Platform.e
  type h = [
      | `Connect of Client_id.t * int
      | `Disconnect of Client_id.t * Sexp.t
      | `Message of Client_id.t * m
    ]

  let echo_request : int option -> Message.t =
    let body = Cstruct.create 0 in
    fun v ->
      match v with
        | None    -> assert false
        | Some(v) ->
          let open Header in
          { version = v; type_code = type_code_echo_request; length = size; xid = 0l }, body

  module Handler = struct
    let connect (t:t) (c_id:Client_id.t) =
      ClientTbl.add_exn t.clients c_id (Conn.create ())

    let handshake (t:t) (c_id:Client_id.t) (version:int) =
      ClientTbl.change t.clients c_id (function
        | None       -> assert false
        | Some(conn) -> Some(Conn.complete_handshake conn version))

    let activity (t:t) (c_id:Client_id.t) =
      ClientTbl.change t.clients c_id (function
        | None       -> assert false
        | Some(conn) -> Some(Conn.activity conn))

    let idle (t:t) (c_id:Client_id.t) (span : Time.Span.t) =
      ClientTbl.change t.clients c_id (function
        | None       -> assert false
        | Some(conn) ->
          let conn', change = Conn.idle conn span in
          if change then begin
            printf "client %s marked as idle... probing\n%!" (Client_id.to_string c_id);
            let echo_req = echo_request conn'.Conn.version in
            let result = Result.try_with (fun () ->
              Platform.send_ignore_errors t.platform c_id echo_req) in
            match result with
              | Error exn ->
                printf "client %s write failed: %s\n%!"
                  (Client_id.to_string c_id) (Exn.to_string exn);
                Platform.close t.platform c_id
              | Ok () -> ()
          end;
          Some(conn'))

    let kill (t:t) (c_id:Client_id.t) (span : Time.Span.t) =
      ClientTbl.change t.clients c_id (function
        | None       -> assert false
        | Some(conn) ->
          let conn', change = Conn.kill conn span in
          if change then begin
            printf "client %s killed\n%!" (Client_id.to_string c_id);
            Platform.close t.platform c_id;
          end;
          Some(conn'))
  end

  module Mon = struct
    let rec monitor t f =
      after t.monitor_interval >>> fun () ->
      ClientTbl.iter t.clients (fun ~key:c_id ~data:_ -> f t c_id);
      monitor t f

    let rec mark_idle t =
      monitor t (fun t c_id -> Handler.idle t c_id t.idle_wait)

    let rec kill_idle t =
      monitor t (fun t c_id -> Handler.kill t c_id t.kill_wait)
  end

  let set_monitor_interval (t:t) (s:Time.Span.t) : unit =
    t.monitor_interval <- s

  let set_idle_wait (t:t) (s:Time.Span.t) : unit =
    t.idle_wait <- s

  let set_kill_wait (t:t) (s:Time.Span.t) : unit =
    t.kill_wait <- s

  let create ?max_pending_connections
      ?verbose
      ?log_disconnects
      ?buffer_age_limit
      ?(monitor_connections=false) ~port () =
    Platform.create ?max_pending_connections ?verbose ?log_disconnects
      ?buffer_age_limit ~monitor_connections ~port ()
    >>| function t ->
      let ctl = {
        platform = t;
        clients = ClientTbl.create ();
        monitor_interval = Time.Span.of_ms 500.0;
        idle_wait = Time.Span.of_sec 5.0;
        kill_wait = Time.Span.of_sec 3.0;
      } in
      if monitor_connections then begin
        Mon.mark_idle ctl;
        Mon.kill_idle ctl
      end;
      ctl

  let listen t = Platform.listen t.platform

  let close t c_id =
    Platform.close t.platform c_id

  let has_client_id t c_id =
    Platform.has_client_id t.platform c_id &&
      match ClientTbl.find t.clients c_id with
        | Some({ Conn.version = Some(_) }) -> true
        | _                                -> false

  let send t c_id m =
    Platform.send t.platform c_id m
    >>| function
      | `Sent x -> Handler.activity t c_id; `Sent x
      | `Drop x -> `Drop x

  let send_ignore_errors t = Platform.send_ignore_errors t.platform

  let send_to_all t = Platform.send_to_all t.platform
  let client_addr_port t = Platform.client_addr_port t.platform
  let listening_port t = Platform.listening_port t.platform

  let handshake v t evt =
    let open Header in
    match evt with
      | `Connect c_id ->
        Handler.connect t c_id;
        let header = { version = v; type_code = type_code_hello;
                       length = size; xid = 0l; } in
        Platform.send t.platform c_id (header, Cstruct.of_string "")
        (* XXX(seliopou): This swallows any errors that might have occurred
         * while attemping the handshake. Any such error should not be raised,
         * since as far as the user is concerned the connection never existed.
         * At the very least, the exception should be logged, which it will be
         * as long as the log_disconnects option is not disabled when creating
         * the controller.
         * *)
        >>| (function _ -> [])
      | `Message (c_id, msg) ->
        begin match ClientTbl.find t.clients c_id with
          | None -> assert false
          | Some({ Conn.version = None }) ->
            let hdr, bits = msg in
            begin
              if not (hdr.type_code = type_code_hello) then begin
                close t c_id;
                raise (Handshake (c_id, Printf.sprintf
                          "Expected 0 code in header: %s%!"
                          (Header.to_string hdr)))
              end
            end;
            Handler.handshake t c_id (min hdr.version v);
            return [`Connect (c_id, min hdr.version v)]
          | Some(_) ->
            Handler.activity t c_id;
            return [`Message (c_id, msg)]
        end
      | `Disconnect (c_id, exn) ->
        begin match ClientTbl.find t.clients c_id with
          | None -> assert false
          | Some({ Conn.version = None }) ->
            ClientTbl.remove t.clients c_id;
            return []
          | Some(_) ->
            ClientTbl.remove t.clients c_id;
            return [`Disconnect (c_id, exn)]
        end

  let echo t evt =
    let open Header in
    match evt with
      | `Message (c_id, (hdr, bytes)) ->
        begin if hdr.Header.type_code = type_code_echo_request then
          (* Echo requests get a reply *)
          let hdr = { hdr with type_code = type_code_echo_reply } in
          send t c_id (hdr , bytes)
          (* XXX(seliopou): This swallows any errors that might have occurred
           * while attemping the handshake. Any such error should not be raised,
           * since as far as the user is concerned the connection never existed.
           * At the very least, the exception should be logged, which it will be
           * as long as the log_disconnects option is not disabled when creating
           * the controller.
           * *)
          >>| (function _ -> [])
        else if hdr.Header.type_code = type_code_echo_reply then begin
          (* Echo replies get eaten, after recording activity. *)
          Handler.activity t c_id;
          return []
        end else
          (* All other messages get forwarded *)
          return [evt]
        end
      | _ -> return [evt]
end

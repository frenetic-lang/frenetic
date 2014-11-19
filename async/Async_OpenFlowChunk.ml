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
  module Client_id = struct
    module T = struct
      type t = Platform.Client_id.t with sexp
      let compare = compare
      let hash = Hashtbl.hash
    end
    include T
    include Hashable.Make(T)
  end

  module Xid = Int32
  module XidTbl = Hashtbl.Make(Int32)

  exception Handshake of Client_id.t * string
  exception Duplicate_xid of Xid.t

  module Conn = struct
    type r = [ `Disconnect of Sexp.t | `Result of Message.t ]

    type t = {
      state : [ `Active | `Idle | `Kill ];
      version : int option;
      mutable xid : int32;
      txns : r Ivar.t XidTbl.t;
      state_entered : Time.t;
      last_activity : Time.t
    }

    let create () : t =
      let now = Time.now () in
      { state = `Active
      ; version = None
      ; xid = 1l
      ; txns = XidTbl.create ()
      ; state_entered = now
      ; last_activity = now
      }

    let next_xid (t:t) =
      let xid = t.xid in
      t.xid <- Int32.(xid + 1l);
      xid

    let add_txn (t:t) m =
      let xid = (Message.header_of m).OpenFlow_Header.xid in
      let ivar = ref None in
      if not (xid = 0l) then
        XidTbl.change t.txns xid (function
          | None    -> let ivar = ref (Some(Ivar.create ())) in !ivar
          | Some(_) -> None);
      match !ivar with
      | Some(ivar) -> ivar
      | None       -> raise (Duplicate_xid xid)

    let remove_txn (t:t) m =
      let xid = (Message.header_of m).OpenFlow_Header.xid in
      XidTbl.remove t.txns xid

    let match_txn (t:t) m =
      let xid = (Message.header_of m).OpenFlow_Header.xid in
      match xid, XidTbl.find_and_remove t.txns xid with
      | 0l, _
      | _ , None ->
        `Unmatched
      | _, Some(ivar) ->
        Ivar.fill ivar (`Result m);
        `Matched

    let clear_txns (t:t) exn_ =
      XidTbl.iter_vals t.txns ~f:(fun ivar -> Ivar.fill ivar (`Disconnect exn_));
      XidTbl.clear t.txns

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
    clients  : Conn.t Client_id.Table.t;
    mutable monitor_interval : Time.Span.t;
    mutable idle_wait : Time.Span.t;
    mutable kill_wait : Time.Span.t;
  }

  type m = Platform.m
  type c = unit
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
      Client_id.Table.add_exn t.clients c_id (Conn.create ())

    let handshake (t:t) (c_id:Client_id.t) (version:int) =
      Client_id.Table.change t.clients c_id (function
        | None       -> assert false
        | Some(conn) -> Some(Conn.complete_handshake conn version))

    let activity (t:t) (c_id:Client_id.t) =
      Client_id.Table.change t.clients c_id (function
        | None       -> assert false
        | Some(conn) -> Some(Conn.activity conn))

    let idle (t:t) (c_id:Client_id.t) (span : Time.Span.t) =
      Client_id.Table.change t.clients c_id (function
        | None       -> assert false
        | Some(conn) ->
          let conn', change = Conn.idle conn span in
          if change then begin
            printf "client %s marked as idle... probing\n%!" (Platform.Client_id.to_string c_id);
            let echo_req = echo_request conn'.Conn.version in
            let result = Result.try_with (fun () ->
              Platform.send_ignore_errors t.platform c_id echo_req) in
            match result with
              | Error exn ->
                printf "client %s write failed: %s\n%!"
                  (Platform.Client_id.to_string c_id) (Exn.to_string exn);
                Platform.close t.platform c_id
              | Ok () -> ()
          end;
          Some(conn'))

    let kill (t:t) (c_id:Client_id.t) (span : Time.Span.t) =
      Client_id.Table.change t.clients c_id (function
        | None       -> assert false
        | Some(conn) ->
          let conn', change = Conn.kill conn span in
          if change then begin
            printf "client %s killed\n%!" (Platform.Client_id.to_string c_id);
            Platform.close t.platform c_id;
          end;
          Some(conn'))
  end

  module Mon = struct
    let rec monitor t f =
      after t.monitor_interval >>> fun () ->
      Client_id.Table.iter t.clients (fun ~key:c_id ~data:_ -> f t c_id);
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
        clients = Client_id.Table.create ();
        monitor_interval = Time.Span.of_ms 500.0;
        idle_wait = Time.Span.of_sec 5.0;
        kill_wait = Time.Span.of_sec 3.0;
      } in
      if monitor_connections then begin
        Mon.mark_idle ctl;
        Mon.kill_idle ctl
      end;
      ctl

  let close t c_id =
    Platform.close t.platform c_id

  let has_client_id t c_id =
    Platform.has_client_id t.platform c_id &&
      match Client_id.Table.find t.clients c_id with
        | Some({ Conn.version = Some(_) }) -> true
        | _                                -> false

  let send t c_id m =
    Platform.send t.platform c_id m
    >>| function
      | `Sent x -> Handler.activity t c_id; `Sent x
      | `Drop x -> `Drop x

  let send_txn t c_id m =
    let conn = Client_id.Table.find_exn t.clients c_id in
    let ivar = Conn.add_txn conn m in
    send t c_id m
    >>| function
      | `Sent _   -> `Sent (Ivar.read ivar)
      | `Drop exn ->
        Conn.remove_txn conn m;
        `Drop exn

  let send_ignore_errors t = Platform.send_ignore_errors t.platform

  let send_to_all t = Platform.send_to_all t.platform
  let client_addr_port t = Platform.client_addr_port t.platform
  let listening_port t = Platform.listening_port t.platform

  let client_version t c_id =
    match (Client_id.Table.find_exn t.clients c_id).Conn.version with
    | None      -> raise Not_found
    | Some(ver) -> ver

  let client_next_xid t c_id =
    Conn.next_xid (Client_id.Table.find_exn t.clients c_id)

  let handshake v t evt =
    let open Header in
    match evt with
      | `Connect (c_id, ()) ->
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
        begin match Client_id.Table.find t.clients c_id with
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
        begin match Client_id.Table.find t.clients c_id with
          | None -> assert false
          | Some({ Conn.version = None }) ->
            Client_id.Table.remove t.clients c_id;
            return []
          | Some(_) ->
            Client_id.Table.remove t.clients c_id;
            return [`Disconnect (c_id, exn)]
        end

  let echo t evt =
    let open Header in
    match evt with
      | `Message (c_id, (hdr, bytes)) ->
        Handler.activity t c_id;
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
        else if hdr.Header.type_code = type_code_echo_reply then
          (* Echo replies get eaten. The activity has been recorded above. *)
          return []
        else
          (* All other messages get forwarded *)
          return [evt]
        end
      | _ -> return [evt]

  let txn t evt =
    match evt with
      | `Message (c_id, msg) ->
        let conn = Client_id.Table.find_exn t.clients c_id in
        begin match Conn.match_txn conn msg with
        | `Matched   -> return []
        | `Unmatched -> return [evt]
        end
      | `Disconnect(c_id, exn_) ->
        let conn = Client_id.Table.find_exn t.clients c_id in
        Conn.clear_txns conn exn_;
        return [evt]
      | _ ->
        return [evt]

  let listen t =
    let open Async_OpenFlow_Stage in
    run (echo >=> txn) t (Platform.listen t.platform)

end

(** Low-level OpenFlow API.
    The Platform manages connections to switches for the
    controller. It provides functions to send and receive OpenFlow
    messages that do the necessary low-level serialization themselves.

    It is possible and instructive to build a controller directly atop
    [PLATFORM]. But, see [NetCore] for a higher-level abstraction.
*)
open Frenetic_Socket
open Frenetic_Log
open OpenFlow0x01
open OpenFlow0x01_Parser

open Lwt_io
open Lwt_unix
open Lwt_list

exception SwitchDisconnected of switchId
exception UnknownSwitch of switchId
exception UnknownSwitchDisconnected
exception Internal of string

let server_fd : file_descr option ref = ref None

let max_pending = 64

let init_with_fd (fd : file_descr) : unit Lwt.t = match !server_fd with
  | Some _ ->
    raise_lwt (Internal "Platform already initialized")
  | None ->
    server_fd := Some fd;
    Lwt.return ()
      
let init_with_port (p : int) : unit Lwt.t = 
  let fd = socket PF_INET SOCK_STREAM 0 in
  setsockopt fd SO_REUSEADDR true;
  bind fd (ADDR_INET (Unix.inet_addr_any, p));
  listen fd max_pending;
  init_with_fd fd

let get_fd () : file_descr Lwt.t = match !server_fd with
  | Some fd ->
    Lwt.return fd
  | None ->
    raise_lwt (Invalid_argument "Platform not initialized")

(** Receive an OpenFlow message from a raw socket without updating controller state. *)
let rec recv_from_switch_fd (sock : file_descr) : ((xid * message) option) Lwt.t =
  let ofhdr_str = String.create (2 * sizeof_ofp_header) in
  lwt ok = SafeSocket.recv sock ofhdr_str 0 sizeof_ofp_header in
  if not ok then Lwt.return None
  else
    lwt hdr = Lwt.wrap (fun () -> Header.parse (Cstruct.of_string ofhdr_str)) in
    let body_len = hdr.Header.len - sizeof_ofp_header in
    let body_buf = String.create body_len in
    lwt ok = SafeSocket.recv sock body_buf 0 body_len in
    if not ok then Lwt.return None
    else 
      match Message.parse hdr (Cstruct.of_string body_buf) with
      | Some v -> Lwt.return (Some v)
      | None ->
        Log.printf "platform" 
          "in recv_from_switch_fd, ignoring message with code %d\n%!"
          (msg_code_to_int hdr.Header.typ);
        recv_from_switch_fd sock

let send_to_switch_fd (sock : file_descr) (xid : xid) (msg : message) : unit Lwt.t =
  try_lwt
    lwt out = Lwt.wrap2 Message.marshal xid msg in
    let len = String.length out in
    lwt n = write sock out 0 len in
    if n <> len then
      raise_lwt (Internal "[send_to_switch] not enough bytes written")
    else
      Lwt.return ()
  with Unix.Unix_error (err, fn, arg) ->
    Log.printf "platform"
      "in send_to_switch_fd, %s\n%!"
      (Unix.error_message err);
    Lwt.return ()

let switch_fds : (switchId, file_descr) Hashtbl.t = Hashtbl.create 101

let fd_of_switch_id (sw:switchId) : file_descr option =
  try 
    Some (Hashtbl.find switch_fds sw)
  with Not_found -> 
    None

let disconnect_switch (sw:switchId) : unit Lwt.t =
  Log.printf "platform" "disconnect_switch\n%!";
  match fd_of_switch_id sw with 
  | Some fd -> 
    lwt _ = close fd in
    Hashtbl.remove switch_fds sw;
    Lwt.return ()
  | None -> 
    raise_lwt (UnknownSwitch sw)

let shutdown () : unit =
  Log.printf "platform" "shutdown\n%!";
  match !server_fd with
  | Some fd -> 
    Lwt.ignore_result 
      (iter_p 
        (fun fd -> close fd)
        (Hashtbl.fold (fun _ fd l -> fd::l) switch_fds []))
  | None -> ()

let switch_handshake (fd : file_descr) : features Lwt.t =
  lwt _ = send_to_switch_fd fd 0l (Hello (Cstruct.of_string "")) in
  lwt resp = recv_from_switch_fd fd in 
  match resp with 
  | Some (_, Hello _) ->
    lwt _ = send_to_switch_fd fd 0l FeaturesRequest in
    begin
    lwt resp = recv_from_switch_fd fd in 
    match resp with 
    | Some (_,FeaturesReply feats) ->
      Hashtbl.add switch_fds feats.switch_id fd;
      Log.printf "platform" 
        "switch %Ld connected\n%!"
        feats.switch_id;
      Lwt.return feats
    | _ -> 
      raise_lwt (Internal "expected FeaturesReply")
    end
  | _ -> 
    raise_lwt (Internal "expected Hello")

let send_to_switch (sw : switchId) (xid : xid) (msg : message) : unit Lwt.t =
  match fd_of_switch_id sw with 
  | Some fd -> 
    begin 
      try_lwt
        send_to_switch_fd fd xid msg
      with Internal s ->
        lwt _ = disconnect_switch sw in
        raise_lwt (SwitchDisconnected sw)
    end
  | None -> 
    raise_lwt (UnknownSwitch sw)

(* By handling echoes here, we do not respond to echoes during the
 handshake. *)
let rec recv_from_switch (sw : switchId) : (xid * message) Lwt.t =
  match fd_of_switch_id sw with
  | Some fd -> 
    begin 
      lwt resp =
        try_lwt
          recv_from_switch_fd fd
        with 
        | Internal s ->
          begin
            Log.printf "paltform" "disconnecting switch\n%!";
            lwt _ = disconnect_switch sw in
            raise_lwt (SwitchDisconnected sw)
          end
        | UnknownSwitchDisconnected ->
          raise_lwt (SwitchDisconnected sw)
        | exn ->
          Log.printf "platform" "other error\n%!";
          raise_lwt exn in
      match resp with
      | Some (xid,EchoRequest bytes) ->
        send_to_switch sw xid (EchoReply bytes) >>
        recv_from_switch sw
      | Some (xid, msg) -> 
        Lwt.return (xid, msg)
      | None -> 
        raise_lwt (SwitchDisconnected sw)
     end
  | None -> 
    raise_lwt (UnknownSwitch sw)

let rec accept_switch () =
  lwt server_fd = get_fd () in 
  lwt (fd, sa) = accept server_fd in
  Log.printf "platform" "%s connected, handshaking...\n%!"
    (string_of_sockaddr sa);
  (* TODO(arjun): a switch can stall during a handshake, while another
     switch is ready to connect. To be fully robust, this module should
     have a dedicated thread to accept TCP connections, a thread per new
     connection to handle handshakes, and a queue of accepted switches.
     Then, accept_switch will simply dequeue (or block if the queue is
     empty). *)
   try_lwt
     switch_handshake fd
   with UnknownSwitchDisconnected ->
     begin
       lwt _ = close fd in
       Log.printf "platform" "%s disconnected, trying again...\n%!"
         (string_of_sockaddr sa);
       accept_switch ()
     end

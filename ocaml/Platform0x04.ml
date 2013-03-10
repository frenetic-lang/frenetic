open OpenFlow0x04Types
open OpenFlow0x04Parser

open Lwt
open Lwt_io
open Lwt_unix
open Lwt_list

let sprintf = Format.sprintf

module type PLATFORM = sig
  exception SwitchDisconnected of switchId 
  val send_to_switch : switchId -> xid -> message -> unit t
  val recv_from_switch : switchId -> (xid * message) t
  val accept_switch : unit -> features t
end

let string_of_sockaddr (sa:sockaddr) : string = match sa with
  | ADDR_UNIX str -> 
    str
  | ADDR_INET (addr,port) -> 
    sprintf "%s:%d" (Unix.string_of_inet_addr addr) port

module OpenFlowPlatform = struct
  exception SwitchDisconnected of switchId 
  exception UnknownSwitch of switchId
  exception UnknownSwitchDisconnected 
  exception Internal of string

  let server_fd : file_descr option ref = 
    ref None

  let init_with_fd (fd : file_descr) : unit = 
    match !server_fd with
    | Some _ -> 
      raise (Internal "Platform already initialized")
    | None -> 
      server_fd := Some fd

  let init_with_port (p : int) : unit = 
    match !server_fd with
    | Some _ -> 
      raise (Internal "Platform already initialized")
    | None -> 
      let fd = socket PF_INET SOCK_STREAM 0 in
      setsockopt fd SO_REUSEADDR true;
      bind fd (ADDR_INET (Unix.inet_addr_any, p));
      listen fd 64; (* JNF: 640K ought to be enough for anybody. *)
      server_fd := Some fd

  let get_fd () = 
    match !server_fd with
    | Some fd -> 
      fd
    | None -> 
      raise (Invalid_argument "Platform not initialized")

  (** Receives an OpenFlow message from a raw file. Does not update
      any controller state. *)
  let rec recv_from_switch_fd (sock : file_descr) : (xid * message) Lwt.t = 
    let ofhdr_str = String.create (2 * sizeof_ofp_header) in
    lwt b = Util.SafeSocket.recv sock ofhdr_str 0 sizeof_ofp_header in 
    if not b then 
      raise_lwt UnknownSwitchDisconnected
    else  
      let bits = (Cstruct.of_string ofhdr_str) in
      let sizeof_body = (get_ofp_header_length bits) - sizeof_ofp_header in
      let body_str = String.create sizeof_body in
      lwt b = Util.SafeSocket.recv sock body_str 0 sizeof_body in 
      if not b then 
	raise_lwt UnknownSwitchDisconnected
      else
	lwt _ = eprintf "[platform] returning message with code %d\n%!" 
	  (get_ofp_header_typ bits) in
        let bits = Cstruct.of_string (String.concat "" [ofhdr_str; body_str]) in
	lwt msg = Lwt.wrap (fun () -> Message.parse bits) in
  return (get_ofp_header_xid bits, msg)
  
  let send_to_switch_fd (sock : file_descr) (xid : xid) (msg : message) = 
    try_lwt
      let buf = Cstruct.create (Message.sizeof msg) in
      lwt len = Lwt.wrap2 Message.marshal buf msg in
      let out = Cstruct.to_string buf in
      lwt n = write sock out 0 len in
      if n <> len then
        raise_lwt (Internal "[send_to_switch] not enough bytes written")
      else 
        return ()
    with Unix.Unix_error (err, fn, arg) ->
      Lwt_io.eprintf "[platform] error sending: %s (in %s)\n%!"
        (Unix.error_message err) fn

  let switch_fds : (switchId, file_descr) Hashtbl.t = 
    Hashtbl.create 100

  let fd_of_switch_id (switch_id:switchId) : file_descr = 
    try Hashtbl.find switch_fds switch_id 
    with Not_found -> raise (UnknownSwitch switch_id)

  let disconnect_switch (sw_id : switchId) = 
    lwt _ = eprintf "[platform] disconnect_switch\n%!" in
    try_lwt
      let fd = Hashtbl.find switch_fds sw_id in
      lwt _ = close fd in
      Hashtbl.remove switch_fds sw_id;
      return ()
    with Not_found ->
      lwt _ = eprintf "[disconnect_switch] switch not found\n%!" in
      raise_lwt (UnknownSwitch sw_id)

  let shutdown () : unit = 
    let _ = eprintf "[platform] shutdown\n%!" in
    match !server_fd with 
    | Some fd -> 
      ignore_result 
	begin 
	  lwt _ = iter_p (fun fd -> close fd)
            (Hashtbl.fold (fun _ fd l -> fd::l) switch_fds []) in 
	  return ()
	end
    | None -> 
       ()

  let switch_handshake (fd : file_descr) : features Lwt.t = 
    lwt _ = eprintf "[platform] switch_handshake\n%!" in
    lwt _ = send_to_switch_fd fd 0l Hello in
    lwt _ = eprintf "[platform] trying to read Hello\n%!" in
    lwt (xid, msg) = recv_from_switch_fd fd in
    match msg with
      | Hello _ -> 
	lwt _ = eprintf "[platform] sending Features Request\n%!" in 
        lwt _ = send_to_switch_fd fd 0l FeaturesRequest in
        lwt (_, msg) = recv_from_switch_fd fd in
        begin
          match msg with
            | FeaturesReply feats ->
              Hashtbl.add switch_fds feats.datapath_id fd;
              lwt _ = eprintf "[platform] switch %Ld connected\n%!"
                        feats.datapath_id in
              return feats
            | _ -> raise_lwt (Internal "expected FEATURES_REPLY")
        end 
      | _ -> raise_lwt (Internal "expected Hello")

  let send_to_switch (sw_id : switchId) (xid : xid) (msg : message) : unit t = 
    lwt _ = eprintf "[platform] send_to_switch\n%!" in
    let fd = fd_of_switch_id sw_id in
    try_lwt 
      send_to_switch_fd fd xid msg
    with Internal s ->
      lwt _ = disconnect_switch sw_id in
      raise_lwt (SwitchDisconnected sw_id)

  (* By handling echoes here, we do not respond to echoes during the
     handshake. *)
  let rec recv_from_switch (sw_id : switchId) : (xid * message) t = 
    lwt _ = eprintf "[platform] recv_from_switch\n%!" in
    let switch_fd = fd_of_switch_id sw_id in
    lwt (xid, msg) = 
      try_lwt
        recv_from_switch_fd switch_fd
      with 
      | Internal s ->
        begin
	  lwt _ = eprintf "[platform] disconnecting switch\n%!" in 
          lwt _ = disconnect_switch sw_id in
          raise_lwt (SwitchDisconnected sw_id)
        end 
      | UnknownSwitchDisconnected -> 
	  raise_lwt (SwitchDisconnected sw_id)
      | exn -> 
	  lwt _ = eprintf "[platform] other error\n%!" in 
          raise_lwt exn in 
    match msg with
      | EchoRequest bytes -> 
        send_to_switch sw_id xid (EchoReply bytes) >>
        recv_from_switch sw_id
      | _ -> return (xid, msg)

  let rec accept_switch () = 
    lwt _ = eprintf "[platform] accept_switch\n%!" in
    lwt (fd, sa) = accept (get_fd ()) in
    lwt _ = eprintf "[platform] : %s connected, handshaking...\n%!"
      (string_of_sockaddr sa) in
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
         lwt _ = eprintf "[platform] : %s disconnected, trying again...\n%!"
	   (string_of_sockaddr sa) in
         accept_switch ()
       end
      
end

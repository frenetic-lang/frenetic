(** Low OpenFlow API.
    The Platform manages connections to switches for the
    controller. It provides functions to send and receive OpenFlow
    messages that do the necessary low-level serialization themselves.

    It is possible and instructive to build a controller directly atop
    [PLATFORM]. But, see [NetCore] for a higher-level abstraction.
*)
module Log = Frenetic_Log
module Socket = Frenetic_Socket
module OF = OpenFlow0x01
module Message = OF.Message

type switchId = OF.switchId
type xid = OF.Message.xid

exception SwitchDisconnected of switchId

let server_fd : Lwt_unix.file_descr option ref = ref None

let max_pending : int = 64

let switch_fds : (switchId, Lwt_unix.file_descr) Hashtbl.t = Hashtbl.create 101

let init_with_fd (fd : Lwt_unix.file_descr) : unit Lwt.t = 
  match !server_fd with
  | Some _ ->
    raise_lwt (Invalid_argument "Platform already initialized")
  | None ->
    server_fd := Some fd;
    Lwt.return ()

let init_with_port (port : int) : unit Lwt.t = 
  let open Lwt_unix in 
  let fd = socket PF_INET SOCK_STREAM 0 in
  setsockopt fd SO_REUSEADDR true;
  bind fd (ADDR_INET (Unix.inet_addr_any, port));
  listen fd max_pending;
  init_with_fd fd

let get_fd () : Lwt_unix.file_descr Lwt.t = 
  match !server_fd with 
    | Some fd ->
      Lwt.return fd
    | None ->
      raise_lwt (Invalid_argument "Platform not initialized")

(* If this function cannot parse a message, it logs a warning and
   tries to receive and parse the next. *)
let rec recv_from_switch_fd (sock : Lwt_unix.file_descr) 
    : (xid * Message.t) option Lwt.t =
  let ofhdr_str = String.create (2 * Message.Header.size) in (* JNF: why 2x? *)
  lwt ok = Socket.recv sock ofhdr_str 0 Message.Header.size in
  if not ok then 
    Lwt.return None
  else
    lwt hdr = Lwt.wrap (fun () -> Message.Header.parse ofhdr_str) in
    let body_len = Message.Header.len hdr - Message.Header.size in
    let body_buf = String.create body_len in
    lwt ok = Socket.recv sock body_buf 0 body_len in
    if not ok then 
      Lwt.return None
    else try
      let xid, msg = Message.parse hdr body_buf in
      Lwt.return (Some (xid, msg))
    with 
      | OF.Unparsable error_msg ->
        Log.printf "platform" 
          "in recv_from_switch_fd, error parsing incoming message (%s)\n%!"
          error_msg;
        recv_from_switch_fd sock
      | OF.Ignored msg ->
        Log.printf "platform" 
          "in recv_from_switch_fd, ignoring message (%s)\n%!"
          msg;
        recv_from_switch_fd sock

let send_to_switch_fd 
  (sock : Lwt_unix.file_descr) 
  (xid : Message.xid) 
  (msg : Message.t) : unit option Lwt.t =
  lwt msg_buf = Lwt.wrap2 Message.marshal xid msg in
  let msg_len = String.length msg_buf in
  lwt sent = Lwt_unix.write sock msg_buf 0 msg_len in
  if sent <> msg_len then
    Lwt.return None
  else
    Lwt.return (Some ())

let fd_of_switch_id (sw : switchId) : Lwt_unix.file_descr option =  
  try
    Some (Hashtbl.find switch_fds sw)
  with Not_found -> 
    None

let disconnect_switch (sw : switchId) : unit Lwt.t = 
  match fd_of_switch_id sw with 
  | Some fd -> 
    lwt _ = Lwt_unix.close fd in
    Hashtbl.remove switch_fds sw;
    Lwt.return ()
  | None -> 
    Lwt.return ()
      
let shutdown () : unit = 
  Lwt.ignore_result 
    (lwt fd = get_fd () in 
     lwt _ = Lwt_unix.close fd in 
     Lwt_list.iter_p (* it is okay to discard these exceptions *)
       Lwt_unix.close
       (Hashtbl.fold (fun _ fd l -> fd::l) switch_fds []))

let send_to_switch 
  (sw : switchId) 
  (xid : Message.xid) 
  (msg : Message.t) : unit Lwt.t =
  match fd_of_switch_id sw with 
  | Some fd ->
    lwt ok = send_to_switch_fd fd xid msg in 
    begin match ok with 
      | Some () -> 
        Lwt.return ()
      | None -> 
        lwt _ = disconnect_switch sw in 
        raise_lwt (SwitchDisconnected sw)
    end
  | None ->
    raise_lwt (SwitchDisconnected sw)

let rec recv_from_switch (sw : switchId) : (Message.xid * Message.t) Lwt.t = 
  let open Message in
  match fd_of_switch_id sw with
    | Some fd -> 
      begin 
        lwt resp = recv_from_switch_fd fd in 
        match resp with
          | Some (xid, EchoRequest bytes) ->
            send_to_switch sw xid (EchoReply bytes) >>
            recv_from_switch sw
          | Some (xid, msg) -> 
            Lwt.return (xid, msg)
          | None -> 
            raise_lwt (SwitchDisconnected sw)
      end
    | None -> 
      raise_lwt (SwitchDisconnected sw)
        
let switch_handshake (fd : Lwt_unix.file_descr) : 
  OF.SwitchFeatures.t option Lwt.t =
  let open Message in
  lwt ok = send_to_switch_fd fd 0l (Hello (Cstruct.of_string "")) in
  match ok with 
    | Some () -> 
      lwt resp = recv_from_switch_fd fd in 
      begin match resp with 
        | Some (_, Hello _) ->
          begin 
            lwt ok = send_to_switch_fd fd 0l SwitchFeaturesRequest in
            match ok with 
              | Some () -> 
                begin 
                  lwt resp = recv_from_switch_fd fd in 
                  match resp with 
                    | Some (_,SwitchFeaturesReply feats) ->
                      Hashtbl.add switch_fds feats.OF.SwitchFeatures.switch_id fd;
                      Log.printf "Platform"  "switch %Ld connected\n%!"
                        feats.OF.SwitchFeatures.switch_id;
                      Lwt.return (Some feats)
                    | _ ->
                      Lwt.return None
                end
              | None ->
                Lwt.return None
          end
        | Some (_, error) ->
          let open OF.Error in
          begin match error with
          | ErrorMsg HelloFailed (code, bytes) ->
            let open HelloFailed in
            begin match code with
            | Incompatible -> 
              Log.printf "platform" "OFPET_HELLO_FAILED received (code: OFPHFC_INCOMPATIBLE)!\n";
              Lwt.return None
            | Eperm -> 
              Log.printf "platform" "OFPET_HELLO_FAILED received (code: OFPHFC_EPERM)!\n";
              Lwt.return None
            end
          | _ -> Lwt.return None
          end
        | None ->
          Lwt.return None
      end 
    | None -> 
      Lwt.return None

(* TODO(arjun): a switch can stall during a handshake, while another
   switch is ready to connect. To be fully robust, this module should
   have a dedicated thread to accept TCP connections, a thread per new
   connection to handle handshakes, and a queue of accepted switches.
   Then, accept_switch will simply dequeue (or block if the queue is
   empty). *)
let rec accept_switch () =
  lwt server_fd = get_fd () in 
  lwt (fd, sa) = Lwt_unix.accept server_fd in
  lwt ok = switch_handshake fd in 
  match ok with 
    | Some feats -> Lwt.return feats
    | None -> accept_switch ()
        
        

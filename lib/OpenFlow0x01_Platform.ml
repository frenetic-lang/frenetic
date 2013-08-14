(** Low OpenFlow API.
    The Platform manages connections to switches for the
    controller. It provides functions to send and receive OpenFlow
    messages that do the necessary low-level serialization themselves.

    It is possible and instructive to build a controller directly atop
    [PLATFORM]. But, see [NetCore] for a higher-level abstraction.
*)
module Log = Lwt_log
module Socket = Frenetic_Socket
module OF = OpenFlow0x01
module Message = OF.Message
module Switch = OpenFlow0x01_switch

type switchId = OF.switchId
type xid = OF.xid

exception SwitchDisconnected of switchId

let server_fd : Lwt_unix.file_descr option ref = ref None

let max_pending : int = 64

let switch_handles : (switchId, Switch.t) Hashtbl.t = Hashtbl.create 101

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

let handle_of_switch_id (sw : switchId) : Switch.t option =  
  try
    Some (Hashtbl.find switch_handles sw)
  with Not_found -> 
    None
      
let shutdown () : unit = 
  Lwt.ignore_result 
    (lwt fd = get_fd () in 
     lwt _ = Lwt_unix.close fd in 
     Lwt_list.iter_p (* it is okay to discard these exceptions *)
       Switch.disconnect
       (Hashtbl.fold (fun _ h l -> h::l) switch_handles []))

let send_to_switch 
  (sw : switchId) 
  (xid : xid) 
  (msg : Message.t) : unit Lwt.t =
  match handle_of_switch_id sw with 
  | Some handle -> Switch.send handle xid msg
  | None -> raise_lwt (Switch.Disconnected sw)

let rec recv_from_switch (sw : switchId) : (xid * Message.t) Lwt.t = 
  match handle_of_switch_id sw with
  | Some handle -> Switch.recv handle
  | None -> raise_lwt (Switch.Disconnected sw)


(* TODO(arjun): a switch can stall during a handshake, while another
   switch is ready to connect. To be fully robust, this module should
   have a dedicated thread to accept TCP connections, a thread per new
   connection to handle handshakes, and a queue of accepted switches.
   Then, accept_switch will simply dequeue (or block if the queue is
   empty). *)
let rec accept_switch () =
  lwt server_fd = get_fd () in 
  lwt (fd, sa) = Lwt_unix.accept server_fd in
  match_lwt Switch.handshake fd with
  | Some handle -> 
    Hashtbl.add switch_handles (Switch.id handle) handle;
    Lwt.return (Switch.features handle)
  | None ->
    accept_switch ()

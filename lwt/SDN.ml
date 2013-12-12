module Switch = HighLevelSwitch

type switchId = SDN_Types.switchId
type flowTable = SDN_Types.flowTable
type switchFeatures = SDN_Types.switchFeatures

let switches : (switchId, Switch.t) Hashtbl.t = Hashtbl.create 100

(* TODO(arjun): a switch can stall during a handshake, while another
   switch is ready to connect. To be fully robust, this module should
   have a dedicated thread to accept TCP connections, a thread per new
   connection to handle handshakes, and a queue of accepted switches.
   Then, accept_switch will simply dequeue (or block if the queue is
   empty). *)
let rec accept_switch
  (server_fd : Lwt_unix.file_descr)
  (push_switch : switchFeatures option -> unit) : unit Lwt.t =
  lwt (fd, sa) = Lwt_unix.accept server_fd in
  match_lwt Switch.initialize fd with
  | Some sw -> 
    let features = Switch.features sw in
    Hashtbl.add switches features.SDN_Types.switch_id sw;
    push_switch (Some features);
    accept_switch server_fd push_switch
  | None ->
    accept_switch  server_fd push_switch

(* Number of waiting switches. *)
let max_pending : int = 64

let accept_switches (port : int) : (unit Lwt.u * switchFeatures Lwt_stream.t) Lwt.t =
  let open Lwt_unix in
  let fd = socket PF_INET SOCK_STREAM 0 in
  setsockopt fd SO_REUSEADDR true;
  bind fd (ADDR_INET (Unix.inet_addr_any, port));
  listen fd max_pending;
  let (kill_thread, kill_wakener) = Lwt.wait () in
  let (switch_stream, push_switch) = Lwt_stream.create () in
  Lwt.async (fun () -> 
    Lwt.pick [ kill_thread; accept_switch fd push_switch ]);
  Lwt.return (kill_wakener, switch_stream)

let get_switch (swId : switchId) : Switch.t Lwt.t =
  Lwt.wrap2 Hashtbl.find switches swId

let setup_flow_table (swId : switchId) tbl =
  lwt sw = get_switch swId in
  Switch.setup_flow_table sw tbl

let flow_stats_request (swId : switchId) pat =
  lwt sw = get_switch swId in
  Switch.flow_stats_request sw pat

let packet_in (swId : switchId) =
  Switch.packet_in (Hashtbl.find switches swId)

let packet_out (swId : switchId) payload action =
  lwt sw = get_switch swId in
  Switch.packet_out sw payload action

let features (swId : switchId) =
  Switch.features (Hashtbl.find switches swId)

let disconnect (swId : switchId) =
  lwt sw = get_switch swId in
  Switch.disconnect sw  

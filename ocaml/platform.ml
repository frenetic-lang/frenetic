open MessagesDef
open Unix
open Printf
open Openflow1_0

type switchId = int64

module type PLATFORM = sig

  exception SwitchDisconnected of switchId

  val send_to_switch : switchId -> xid -> message -> unit

  val recv_from_switch : switchId -> xid * message

  val accept_switch : unit -> features

end

module TestPlatform = struct

  type status = Connecting | Connected | Disconnected

  type switch = {
    mutable status : status;
    to_controller : (xid * message) Event.channel;
    to_switch : (xid * message) Event.channel;    
  }

  let switches : (switchId, switch) Hashtbl.t = Hashtbl.create 100

  let pending_switches : (switchId * switch) Event.channel = 
    Event.new_channel ()

  exception SwitchDisconnected of switchId

  let destroy_switch (id : switchId) (sw : switch) : unit = 
    let rec drain_to_controller () = 
      match Event.poll (Event.receive sw.to_controller) with
        | Some (xid,msg) -> 
          eprintf "message to controller from %Ld not consumed.\n%!" id;
          drain_to_controller ()
        | None -> () in
    let rec drain_to_switch () = 
      match Event.poll (Event.receive sw.to_switch) with
        | Some (xid,msg) -> 
          eprintf "message to switch from %Ld not consumed.\n%!" id;
          drain_to_switch () 
        | None -> () in
    drain_to_controller ();
    drain_to_switch ();
    sw.status <- Disconnected
      
  let tear_down () = 
    Hashtbl.iter destroy_switch switches;
    let rec kill_pending_switches () = 
      match Event.poll (Event.receive pending_switches) with
        | Some (id, sw) -> destroy_switch id sw; kill_pending_switches ()
        | None -> () in
    kill_pending_switches ()

  let connect_switch sw_id = 
    let sw = { status = Connecting; 
               to_controller = Event.new_channel (); 
               to_switch = Event.new_channel () } in
    if Hashtbl.mem switches sw_id then
      failwith "already connected"
    else
      begin
        let _ = Event.sync (Event.send pending_switches (sw_id, sw)) in
        ()
      end

  let disconnect_switch sw_id = 
    let sw = Hashtbl.find switches sw_id in
    sw.status <- Disconnected

  let accept_switch () = 
    let (sw_id, sw) = Event.sync (Event.receive pending_switches) in
    sw.status <- Connected;
    Hashtbl.add switches sw_id sw;
    { switch_id = sw_id;
      num_buffers = 100l;
      num_tables = 1;
      (* This is an amazing switch! *)
      supported_capabilities = 
        { flow_stats = true;
          table_stats = true;
          port_stats = true; 
          stp = true;
          ip_reasm = true;
          queue_stats = true; 
          arp_match_ip = true };
      supported_actions =
        { output = true;
          set_vlan_id = true; 
          set_vlan_pcp = true;
          strip_vlan = true; 
          set_dl_src = true;
          set_dl_dst = true;
          set_nw_src = true; 
          set_nw_dst = true; 
          set_nw_tos = true;
          set_tp_src = true; 
          set_tp_dst = true;
          enqueue = true;
          vendor = false } }

  let exn_if_disconnected sw_id sw = 
    if sw.status = Disconnected then
      begin
        Hashtbl.remove switches sw_id;
        raise (SwitchDisconnected sw_id)
      end
     
  let send_to_switch sw_id xid msg = 
    let sw = Hashtbl.find switches sw_id in
    exn_if_disconnected sw_id sw;
    Event.sync (Event.send sw.to_switch (xid, msg))

  let recv_from_switch sw_id = 
    let sw = Hashtbl.find switches sw_id in
    exn_if_disconnected sw_id sw;
    (* The controller is receiving messages from the switch. *)
    Event.sync (Event.receive sw.to_controller)

  let fail_if_disconnected sw_id sw = 
    if sw.status = Disconnected then
      begin
        Hashtbl.remove switches sw_id;
        failwith "dude, the switch is sending after it disconnected ..."
      end

  let send_to_controller sw_id xid msg = 
    let sw = Hashtbl.find switches sw_id in
    fail_if_disconnected sw_id sw;
    Event.sync (Event.send sw.to_controller (xid, msg))
    
  let recv_from_controller sw_id = 
    let sw = Hashtbl.find switches sw_id in
    exn_if_disconnected sw_id sw;
    Event.sync (Event.receive sw.to_switch)

end

let string_of_sockaddr (sa : sockaddr) : string = match sa with
  | ADDR_UNIX str -> str
  | ADDR_INET (addr,port) -> sprintf "%s:%d" (string_of_inet_addr addr) port

(* TODO(arjun): awful performance--how do I use the newer cstruct on Github? *)
let ba_of_string (str : string) : Cstruct.buf = 
  let open Bigarray in
  let len = String.length str in
  let ba = Array1.create char c_layout len in
  for i = 0 to len - 1 do
    Array1.set ba i (String.get str i)
  done;
  ba


module OpenFlowPlatform = struct

  let server_fd : file_descr option ref = ref None

  let init_with_fd (fd : file_descr) : unit = match !server_fd with
    | Some _ -> raise (Invalid_argument "Platform already initialized")
    | None -> server_fd := Some fd

  let init_with_port (p : int) : unit = match !server_fd with
    | Some _ -> raise (Invalid_argument "Platform already initialized")
    | None -> 
      let fd = socket PF_INET SOCK_STREAM 0 in
      bind fd (ADDR_INET (inet_addr_any, p));
      listen fd 10;
      at_exit (fun () -> close fd);
      server_fd := Some fd

  let get_fd () = match !server_fd with
    | Some fd -> fd
    | None -> raise (Invalid_argument "Platform not initialized")

  exception Internal of string
      
  (** Receives an OpenFlow message from a raw file. Does not update
      any controller state. *)
  let recv_from_switch_fd (fd : file_descr) : xid * message = 
    eprintf "creating header \n%!";
    let ofhdr_str = String.create (2 * sizeof_ofp_header) in
    let n = read fd ofhdr_str 0 sizeof_ofp_header in
    if n <> sizeof_ofp_header then
      begin
        eprintf "[recv_from_switch] not enough bytes read\n%!";
        raise (Internal "not enough bytes read")
      end;
    let hdr = Header.parse (ba_of_string ofhdr_str) in
    let sizeof_body = hdr.Header.len - sizeof_ofp_header in
    let body_str = String.create sizeof_body in
    let n = read fd body_str 0 sizeof_body in
    if n <> sizeof_body then
      begin 
        eprintf "[recv_from_switch] not enough bytes read from body\n%!";
        raise (Internal "not enough bytes read")
      end;
    Message.parse hdr (ba_of_string body_str)

  let send_to_switch_fd (fd : file_descr) (xid : xid) (msg : message) : unit = 
    let out = Message.marshal xid msg in
    let len = String.length out in
    eprintf "[platform] sending a message of length %d, code %d\n" len
      (msg_code_to_int (Message.msg_code_of_message msg));
    try
      let n = write fd out 0 len in
      if n <> len then
        begin
          eprintf "[send_to_switch] not enough bytes written\n%!";
          raise (Internal "not enough bytes written")
        end;
      ()
    with Unix.Unix_error (err, fn, arg) ->
      eprintf "[platform] error sending: %s (in %s)\n%!"
        (Unix.error_message err)
        fn
      


  exception SwitchDisconnected of switchId
  exception UnknownSwitch of switchId


  let switch_fds : (switchId, file_descr) Hashtbl.t = Hashtbl.create 100

  let fd_of_switch_id (switch_id : switchId) : file_descr = 
    try 
      Hashtbl.find switch_fds switch_id 
    with Not_found -> raise (UnknownSwitch switch_id)

  let disconnect_switch (sw_id : switchId) = 
    try 
      let fd = Hashtbl.find switch_fds sw_id in
      close fd;
      Hashtbl.remove switch_fds sw_id        
    with Not_found ->
      eprintf "[disconnect_switch] switch not found\n%!";
      raise (UnknownSwitch sw_id)

  let switch_handshake (fd : file_descr) : features = 
    send_to_switch_fd fd 0l (Hello (ba_of_string ""));
    let (xid, msg) = recv_from_switch_fd fd in
    begin 
      match msg with
        | Hello _ -> ()
        | _ -> raise (Internal "expected Hello")
    end;
    send_to_switch_fd fd 0l FeaturesRequest;
    let (_, msg) = recv_from_switch_fd fd in
    match msg with
      | FeaturesReply feats ->
        Hashtbl.add switch_fds feats.switch_id fd;
        eprintf "[platform] switch %Ld connected\n%!" feats.switch_id;
        feats
      | _ -> raise (Internal "expected FEATURES_REPLY")

  let accept_switch () = 
    let (fd, sa) = accept (get_fd ()) in
    eprintf "[platform] : %s connected, handshaking...\n%!" 
      (string_of_sockaddr sa);
    (* TODO(arjun): a switch can stall during a handshake, while another
       switch is ready to connect. To be fully robust, this module should
       have a dedicated thread to accept TCP connections, a thread per new
       connection to handle handshakes, and a queue of accepted switches.
       Then, accept_switch will simply dequeue (or block if the queue is
       empty). *)
    switch_handshake fd

  let send_to_switch (sw_id : switchId) (xid : xid) (msg : message) : unit = 
    let fd = fd_of_switch_id sw_id in
    try send_to_switch_fd fd xid msg
    with Internal s ->
      begin
        disconnect_switch sw_id;
        raise (SwitchDisconnected sw_id)
      end

  (* By handling echoes here, we do not respond to echoes during the
     handshake. *)
  let rec recv_from_switch (sw_id : switchId) : xid * message = 
    let switch_fd = fd_of_switch_id sw_id in
    let (xid, msg) = 
      try
        recv_from_switch_fd switch_fd
      with Internal s ->
        begin
          disconnect_switch sw_id;
          raise (SwitchDisconnected sw_id)
        end in
    match msg with
      | EchoRequest bytes -> 
        send_to_switch sw_id xid (EchoReply bytes);
        recv_from_switch sw_id
      | _ -> (xid, msg)




      
end

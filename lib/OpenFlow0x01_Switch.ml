module Socket = Frenetic_Socket
module OF = OpenFlow0x01
module OFC = OpenFlow0x01_Core
module Message = OF.Message
module Log = Lwt_log

type file_descr = Lwt_unix.file_descr
type xid = OFC.xid
type msg = Message.t

(* If this function cannot parse a message, it logs a warning and
   tries to receive and parse the next. *)
let rec recv_from_switch_fd (sock : file_descr) : (xid * msg) option Lwt.t =
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
        Log.error_f
          "in recv_from_switch, error parsing incoming message (%s)\n%!"
          error_msg >>
        recv_from_switch_fd sock
      | OF.Ignored msg ->
        Log.error_f
          "in recv_from_switch, ignoring message (%s)\n%!"
          msg >>
        recv_from_switch_fd sock

let send_to_switch_fd (sock : file_descr) (xid : xid) (msg : msg) : bool Lwt.t =
  lwt msg_buf = Lwt.wrap2 Message.marshal xid msg in
  let msg_len = String.length msg_buf in
  lwt sent = Lwt_unix.write sock msg_buf 0 msg_len in
  Lwt.return (sent = msg_len)

let switch_handshake (fd : file_descr) : OF.SwitchFeatures.t option Lwt.t =
  let open Message in
  match_lwt send_to_switch_fd fd 0l (Hello (Cstruct.of_string "")) with
  | true -> 
    lwt resp = recv_from_switch_fd fd in 
    begin match resp with 
      | Some (_, Hello _) ->
        begin 
          match_lwt send_to_switch_fd fd 0l SwitchFeaturesRequest with
            | true ->
              begin 
                lwt resp = recv_from_switch_fd fd in 
                match resp with 
                  | Some (_,SwitchFeaturesReply feats) ->
                    Lwt.return (Some feats)
                  | _ -> Lwt.return None
              end
            | false ->
              Lwt.return None
        end
      | Some (_, error) ->
        let open OF.Error in
        begin match error with
        | ErrorMsg HelloFailed (code, bytes) ->
          let open HelloFailed in
          begin match code with
          | Incompatible -> 
            Log.error_f
              "OFPET_HELLO_FAILED received (code: OFPHFC_INCOMPATIBLE)!\n" >>
            Lwt.return None
          | Eperm -> 
            Log.error_f
              "OFPET_HELLO_FAILED received (code: OFPHFC_EPERM)!\n" >>
            Lwt.return None
          end
        | _ -> Lwt.return None
        end
      | None ->
        Lwt.return None
    end 
  | false -> 
    Lwt.return None

exception Disconnected of OFC.switchId

type t = {
  fd : file_descr;
  features : OF.SwitchFeatures.t;
  send_stream : (xid * msg) Lwt_stream.t;
  send : (xid * msg) option -> unit;
  recv_stream : (xid * msg) Lwt_stream.t;
  recv : (xid * msg) option -> unit
}

let disconnect (switch : t) : unit Lwt.t =
	lwt _ = Lwt_unix.close switch.fd in
	switch.send None;
	switch.recv None;
	Lwt.return ()

let rec recv_thread (switch : t) : unit Lwt.t =
	match_lwt recv_from_switch_fd switch.fd with
  | None ->
    lwt _ = disconnect switch in
	  Lwt.return ()
	| Some (xid, Message.EchoRequest bytes) ->
	  switch.send (Some (xid, Message.EchoReply bytes));
    recv_thread switch
  | Some xid_msg ->
		switch.recv (Some xid_msg); 
    recv_thread switch

let rec send_thread (switch : t) : unit Lwt.t =
	(* only send_thread should ever close send_stream *)
	lwt (xid, msg) = Lwt_stream.next switch.send_stream in
  match_lwt send_to_switch_fd switch.fd xid msg with
  | true -> send_thread switch
  | false -> disconnect switch

let switch_thread (switch : t) : unit Lwt.t =
	Lwt.choose [ send_thread switch; recv_thread switch ]

let handshake (fd : file_descr) : t option Lwt.t = 
  match_lwt switch_handshake fd with
  | None ->
  	Lwt.return None
  | Some features -> 
    let (send_stream, send) = Lwt_stream.create () in
    let (recv_stream, recv) = Lwt_stream.create () in
    let switch = { fd; features; send_stream; send; recv_stream; recv } in
    Lwt.async (fun () -> switch_thread switch);
    Lwt.return (Some switch)

let id (switch : t) =
  switch.features.OF.SwitchFeatures.switch_id

let features (switch : t) =
	switch.features

let send (switch : t) (xid : xid) (msg : msg) : unit Lwt.t =
	try
		switch.send (Some (xid, msg));
		Lwt.return () (* TODO(arjun): Lwt is now pointless here. *)
  with
    Lwt_stream.Closed -> raise (Disconnected (id switch))

let recv (switch : t) : (xid * msg) Lwt.t =
  match_lwt Lwt_stream.get switch.recv_stream with
    | None -> raise (Disconnected (id switch))
    | Some xid_msg -> Lwt.return xid_msg

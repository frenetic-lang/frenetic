module Socket = Socket
module OF = OpenFlow0x01
module OFC = OpenFlow0x01_Core
module Message = OF.Message
module Log = Lwt_log
module Header = OpenFlow_Header

type file_descr = Lwt_unix.file_descr
type xid = OFC.xid
type msg = Message.t


(* If this function cannot parse a message, it logs a warning and
   tries to receive and parse the next. *)
let rec recv_from_switch_fd (sock : file_descr) : (xid * msg) option Lwt.t =
  let ofhdr_str = String.create (2 * Header.size) in (* JNF: why 2x? *)
  match_lwt Socket.recv sock ofhdr_str 0 Header.size with
    | false -> Lwt.return None
    | true ->
      lwt hdr = Lwt.wrap (fun () -> Header.parse (Cstruct.of_string ofhdr_str)) in
      let body_len = hdr.Header.length - Header.size in
      let body_buf = String.create body_len in
      match_lwt Socket.recv sock body_buf 0 body_len with
        | false -> Lwt.return None
        | true ->
          try
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
  try_lwt
    lwt sent = Lwt_unix.write sock msg_buf 0 msg_len in
    Lwt.return (sent = msg_len)
  with Unix.Unix_error (err, fn, arg) ->
    Printf.printf "[platform0x01] error sending: %s (in %s)\n%!"
      (Unix.error_message err) fn;
    Lwt.return (false)

let switch_handshake_finish (fd : file_descr) : OF.SwitchFeatures.t option Lwt.t = 
  match_lwt send_to_switch_fd fd 0l Message.SwitchFeaturesRequest with
    | true ->
      begin match_lwt recv_from_switch_fd fd with 
        | Some (_, Message.SwitchFeaturesReply feats) ->
          Lwt.return (Some feats)
        | _ -> Lwt.return None
      end
    | false ->
      Lwt.return None

let switch_handshake_reply (fd : file_descr) : OF.SwitchFeatures.t option Lwt.t =
  let open Message in
  match_lwt send_to_switch_fd fd 0l (Hello (Cstruct.of_string "")) with
  | true -> 
    begin 
      switch_handshake_finish fd 
    end
  | false -> 
    Lwt.return None
	
let switch_handshake (fd : file_descr) : OF.SwitchFeatures.t option Lwt.t =
  let open Message in
  match_lwt recv_from_switch_fd fd with
  | Some (_, Hello _) ->
    begin
      switch_handshake_reply fd
    end
  | Some (_, error) ->
    let open OF.Error in
    begin match error with
    | ErrorMsg (Error (HelloFailed code, bytes)) ->
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

(******************************************************************************)

type t = {
  fd : file_descr;
  features : OF.SwitchFeatures.t;
  send : (xid * msg) option -> unit;
  recv_stream : (xid * msg) Lwt_stream.t;
  wait_disconnect : unit Lwt.t; (* sleeps until woken by disconnect *)

  disconnect : unit Lwt.u;
}

let id (switch : t) =
  switch.features.OF.SwitchFeatures.switch_id

let features (switch : t) =
  switch.features

let disconnect (switch : t) : unit Lwt.t =
	lwt _ = Lwt_unix.close switch.fd in
  Lwt.wakeup switch.disconnect ();
	Lwt.return ()

let wait_disconnect (switch : t) : unit Lwt.t =
  switch.wait_disconnect

let rec recv_thread 
  (recv : (xid * msg) option -> unit)
  (switch : t) : unit Lwt.t =
	match_lwt recv_from_switch_fd switch.fd with
  | None ->
    disconnect switch
	| Some (xid, Message.EchoRequest bytes) ->
	  switch.send (Some (xid, Message.EchoReply bytes));
    recv_thread recv switch
  | Some xid_msg ->
		recv (Some xid_msg); 
    recv_thread recv switch

let rec send_thread
  (send_stream : (xid * msg) Lwt_stream.t) 
  (switch : t) : unit Lwt.t =
	lwt (xid, msg) = Lwt_stream.next send_stream in
  match_lwt send_to_switch_fd switch.fd xid msg with
  | true -> send_thread send_stream switch
  | false -> disconnect switch

let handshake_aux (hs: file_descr -> OF.SwitchFeatures.t option Lwt.t) (fd : file_descr) 
  : t option Lwt.t 
  = match_lwt hs fd with
  | None ->
    Lwt.return None
  | Some features -> 
    let (send_stream, send) = Lwt_stream.create () in
    let (recv_stream, recv) = Lwt_stream.create () in
    let (wait_disconnect, disconnect) = Lwt.wait () in
    let switch = { fd; 
		   features; 
		   send; 
		   recv_stream;
                   wait_disconnect; 
		   disconnect } in
    Lwt.async (fun () ->
      Lwt.pick [ send_thread send_stream switch;
                 recv_thread recv switch;
                 switch.wait_disconnect ]);
    Lwt.return (Some switch)

let handshake = handshake_aux switch_handshake 

let handshake_reply = handshake_aux switch_handshake_reply

let send (switch : t) (xid : xid) (msg : msg) : unit Lwt.t =
  switch.send (Some (xid, msg));
  Lwt.return () (* TODO(arjun): Lwt is now pointless here. *)

let recv (switch : t) : (xid * msg) Lwt.t =
  Lwt_stream.next switch.recv_stream

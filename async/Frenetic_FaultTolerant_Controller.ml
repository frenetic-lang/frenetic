let read_message (from_client : Reader.t) : 
  [ `Eof | `Message of Frenetic_OpenFlow_Header.xid * OpenFlow0x04.Message.t ] 
  Deferred.t =
  let header_buf = Bytes.create Frenetic_OpenFlow_Header.size in
  Reader.really_read from_client header_buf
  >>= function
  | `Eof _ ->
    Log.info "Connection closed reading header";
    return `Eof
  | `Ok ->
    let header = Frenetic_OpenFlow_Header.parse (Cstruct.of_string header_buf) in
    let message_len = header.length - Frenetic_OpenFlow_Header.size in
    let message_buf = Bytes.create message_len in
    Log.info "Got message %s" (Frenetic_OpenFlow_Header.to_string header);
    Reader.really_read from_client message_buf
    >>= function
    | `Eof _ ->
      Log.info "Error reading client message";
      return `Eof
    | `Ok ->
      return (`Message (OpenFlow0x04.Message.parse header message_buf))

let read_and_respond (from_client : Reader.t) (to_client : Writer.t) 
  (pol : Frenetic_NetKAT.policy) (topo : Frenetic_NetKAT.policy) () 
  : [ `Finished of unit | `Repeat of unit ] Deferred.t =
  read_message from_client
  >>= function
  | `Eof ->
    return (`Finished ())
  | `Message (xid, body) ->
    process_message (send_message to_client) xid body pol topo;
    return (`Repeat ())

let client_handler (from_client : Reader.t) (to_client : Writer.t)
  (pol : Frenetic_NetKAT.policy) (topo : Frenetic_NetKAT.policy) : unit Deferred.t =
  Log.info "Client connected";
  send_message to_client 0l (OpenFlow0x04.Message.Hello [VersionBitMap [0x04]]);
  Log.info "Sent Hello";
  Deferred.repeat_until_finished 
    () (read_and_respond from_client to_client )

let main (of_port : int) (pol_file : string) (topo_file : string) () : unit =
  let pol_str = In_channel.read_all pol_file in
  let pol = Frenetic_NetKAT_Parser.policy_from_string pol_str in
  let topo_str = In_channel.read_all topo_file in
  let topo = _ in
  let _ = Tcp.Server.create ~on_handler_error:`Raise (Tcp.on_port of_port)
            (fun _ reader writer -> client_handler reader writer pol topo) 
  in ()

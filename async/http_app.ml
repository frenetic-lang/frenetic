module NetKAT = NetKAT_Types
module Server = Cohttp_async.Server

module Event = struct
  open NetKAT

  type t = event

  let to_json (event : t) : Yojson.Safe.json =
    match event with
    | PacketIn (pipe, sw_id, pt_id, payload, len) ->
      let buffer = Base64.encode (Cstruct.to_string (SDN_Types.payload_bytes payload)) in
      `Assoc [
          ("type", `String "packet_in");
          ("pipe", `String pipe);
          ("switch_id", `Intlit (Int64.to_string sw_id));
          ("port_id",   `Intlit (Int32.to_string pt_id));
          ("payload",   `Assoc [
              ("buffer", `String buffer);
              ("id", match payload with
                | SDN_Types.Buffered(id, _) -> `Intlit (Int32.to_string id)
                | _                         -> `Null)
          ]);
          ("length",    `Intlit (string_of_int len));
      ]
    | Query (name, pkt_count, byte_count) ->
      `Assoc [
          ("type", `String "query");
          ("packet_count", `Intlit (Int64.to_string pkt_count));
          ("byte_count", `Intlit (Int64.to_string byte_count))]
    | SwitchUp sw_id ->
      `Assoc [
          ("type", `String "switch_up");
          ("switch_id", `Intlit (Int64.to_string sw_id))]
    | SwitchDown sw_id ->
      `Assoc [
          ("type", `String "switch_down");
          ("switch_id", `Intlit (Int64.to_string sw_id))]
    | PortUp (sw_id, pt_id) ->
      `Assoc [
          ("type", `String "port_up");
          ("switch_id", `Intlit (Int64.to_string sw_id));
          ("port_id",   `Intlit (Int32.to_string pt_id))]
    | PortDown (sw_id, pt_id) ->
      `Assoc [
          ("type", `String "port_down");
          ("switch_id", `Intlit (Int64.to_string sw_id));
          ("port_id",   `Intlit (Int32.to_string pt_id))]
    | LinkUp ((sw_id1, pt_id1), (sw_id2, pt_id2)) ->
      `Assoc [
          ("type", `String "link_up");
          ("src", `Assoc [("switch_id", `Intlit (Int64.to_string sw_id1));
                          ("port_id",   `Intlit (Int32.to_string pt_id1))]);
          ("dst", `Assoc [("switch_id", `Intlit (Int64.to_string sw_id2));
                          ("port_id",   `Intlit (Int32.to_string pt_id2))])]
    | LinkDown ((sw_id1, pt_id1), (sw_id2, pt_id2)) ->
      `Assoc [
          ("type", `String "link_down");
          ("src", `Assoc [("switch_id", `Intlit (Int64.to_string sw_id1));
                          ("port_id",   `Intlit (Int32.to_string pt_id1))]);
          ("dst", `Assoc [("switch_id", `Intlit (Int64.to_string sw_id2));
                          ("port_id",   `Intlit (Int32.to_string pt_id2))])]
    | HostUp ((sw_id, pt_id), (dlAddr, nwAddr)) ->
      `Assoc [
          ("type", `String "host_up");
          ("switch_id", `Intlit (Int64.to_string sw_id));
          ("port_id",   `Intlit (Int32.to_string pt_id));
          ("dl_addr",   `String (Packet.string_of_dlAddr dlAddr));
          ("nw_addr",   `String (Packet.string_of_nwAddr nwAddr))]
    | HostDown ((sw_id, pt_id), (dlAddr, nwAddr)) ->
      `Assoc [
          ("type", `String "host_down");
          ("switch_id", `Intlit (Int64.to_string sw_id));
          ("port_id",   `Intlit (Int32.to_string pt_id));
          ("dl_addr",   `String (Packet.string_of_dlAddr dlAddr));
          ("nw_addr",   `String (Packet.string_of_nwAddr nwAddr))]


  let of_json (json : Yojson.Safe.json) : t =
    match json with
    | `Assoc [("type", `String "switch_up");
              ("switch_id", `Intlit sw_id)] ->
      SwitchUp (Int64.of_string sw_id)
    | `Assoc [("type", `String "switch_down");
              ("switch_id", `Intlit sw_id)] ->
      SwitchDown (Int64.of_string sw_id)
    | `Assoc [("type", `String "port_up");
              ("switch_id", `Intlit sw_id);
              ("port_id",   `Intlit pt_id)] ->
      PortUp (Int64.of_string sw_id, Int32.of_string pt_id)
    | `Assoc [("type", `String "port_down");
              ("switch_id", `Intlit sw_id);
              ("port_id",   `Intlit pt_id)] ->
      PortDown (Int64.of_string sw_id, Int32.of_string pt_id)
    | _ ->
      failwith (Printf.sprintf "of_json: unexpected input %s" (Yojson.Safe.to_string json))

  let of_json_string (str : string) : t =
    of_json (Yojson.Safe.from_string str)

  let to_json_string (t : t) : string =
    Yojson.Safe.to_string (to_json t)
end

open Async.Std
open Cohttp_async

let parse_pkt_out body = Body.to_string body >>= fun str ->
  let json = Yojson.Safe.from_string str in
  match json with
    | `Assoc [
       ("data", `Assoc [("actions", `List actions);
       ("port_id", `String port);
       ("switch_id", `String switch);
       ("packet", `String packet)]);
       ("type", `String "packet_out")] ->
        let switch = Int64.of_string switch in
        let port = Int32.of_string port in
        let packet = SDN_Types.NotBuffered(Cstruct.of_string(Base64.decode packet)) in
        let actions = SDN_Types.([Output Flood]) in
      return (switch, (packet, Some (port), actions))

let parse_update body = Body.to_string body >>= fun str ->
  let json = Yojson.Safe.from_string str in
  match json with
  | `Assoc [
      ("data", `String data);
      ("type", `String "policy")] ->
    let lex = Lexing.from_string data in
    let pol = NetKAT_Parser.program NetKAT_Lexer.token lex in
    return pol

let handle_request
  (send : NetKAT_Types.policy Async_NetKAT.send)
  (event_reader : NetKAT_Types.event Pipe.Reader.t)
  ~(body : Cohttp_async.Body.t)
  (client_addr : Socket.Address.Inet.t)
  (request : Request.t) : Cohttp_async.Server.response Deferred.t =
  match request.meth, (Uri.path request.uri) with
    | `GET, "/event" ->
      begin
        Pipe.read event_reader >>= function
        | `Eof -> Cohttp_async.Server.respond `Service_unavailable
        | `Ok evt ->
          let rsp = Event.to_json_string evt in
          Cohttp_async.Server.respond_with_string rsp
      end
    | `POST, "/pkt_out" ->
      parse_pkt_out body >>= fun pkt_out ->
      Pipe.write send.pkt_out pkt_out >>= fun _ ->
      Cohttp_async.Server.respond `OK
    | `POST, "/update" ->
      parse_update body >>= fun pol ->
      Pipe.write send.update pol >>= fun _ ->
      Cohttp_async.Server.respond `OK
    | _, _ -> Cohttp_async.Server.respond `Not_found

let listen ?(port=9000) =
  let pipes = Async_NetKAT.PipeSet.singleton "http" in
  Async_NetKAT.Policy.create_async ~pipes:pipes NetKAT_Types.drop
    (fun topo send () ->
       let (event_reader, event_writer) = Pipe.create () in
       let closed = Cohttp_async.Server.create (Tcp.on_port port)
                      (handle_request send event_reader) in
       fun event ->
         (* TODO(arjun): save event so it can be long-polled later *)
         printf "Adding event to pipe: %s\n%!" (Event.to_json_string event);
         Pipe.write_without_pushback event_writer event;
         return None)

module NetKAT = NetKAT_Types
module Server = Cohttp_async.Server
open NetKAT
open Async.Std
open Cohttp_async

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

let event_to_json_string (t : t) : string =
  Yojson.Safe.to_string (to_json t)

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
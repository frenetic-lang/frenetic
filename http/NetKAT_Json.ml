open NetKAT_Types
open SDN_Types
module NetKAT = NetKAT_Types
module Server = Cohttp_async.Server
open NetKAT
open Async.Std
open Cohttp_async

type t = event

let event_to_json (event : t) : Yojson.Safe.json =
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

(* NOTE: Unused function *)
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

(* NOTE: Unused function *)
let of_json_string (str : string) : t =
  of_json (Yojson.Safe.from_string str)

let event_to_json_string (t : t) : string =
  Yojson.Safe.to_string (event_to_json t)

let parse_payload (json : Yojson.Basic.json) : payload =
  let open Yojson.Basic.Util in
  match json |> member "type" |> to_string with
  | "notbuffered" ->
     let base64 = json |> member "data" |> to_string in
     NotBuffered (Cstruct.of_string (Base64.decode base64))
  | "buffered" ->
    let bufferId = Int32.of_int (json |> member "bufferid" |> to_int) in
    (* TODO(arjun): Why does Buffered take a second argument. Won't it be ignored
       if a buffer ID is specified? *)
    Buffered (bufferId, Cstruct.of_string "")

let to_int32_option (json : Yojson.Basic.json) : Int32.t option =
  let open Yojson.Basic.Util in
  match to_int_option json with
    | None -> None
    | Some n -> Some (Int32.of_int n)

let parse_pseudoport (json : Yojson.Basic.json) : pseudoport =
  let open Yojson.Basic.Util in
  match json |> member "type" |> to_string with
   | "physical" -> Physical (json |> member "port" |> to_int |> Int32.of_int)
   | "inport" -> InPort
   | "table" -> Table
   | "normal" -> Normal
   | "flood" -> Flood
   | "all" -> All
   | "controller" -> Controller (json |> member "bytes" |> to_int)
   | "local" -> Local
   | str -> failwith ("invalid pseudoport type: " ^ str)

let parse_action (json : Yojson.Basic.json) : action =
  let open Yojson.Basic.Util in
  match json |> member "type" |> to_string with
    | "output" -> Output (json |> member "pseudoport"  |> parse_pseudoport)
    | "modify" -> failwith "NYI: parsing modify actions from JSON"
    | "enqueue" -> failwith "NYI: parsing enqueue actions from JSON"
    | str -> failwith ("invalid action type: " ^ str)

(* Using the Basic module because Basic.Util has several handy parsing
   functions *)
let parse_pkt_out' (json : Yojson.Basic.json) : switchId * pktOut =
  let open Yojson.Basic.Util in
  let actions = json |> member "actions" |> to_list |> List.map parse_action in
  let in_port = json |> member "in_port" |> to_int32_option  in
  let switch = json |> member "switch" |> to_int |> Int64.of_int in
  let packet = json |> member "payload" |> parse_payload in
  (switch, (packet, in_port, actions))

let parse_pkt_out body = Body.to_string body >>| fun str ->
  parse_pkt_out' (Yojson.Basic.from_string str)

let parse_update body = Body.to_string body >>= fun str ->
  let json = Yojson.Safe.from_string str in
  match json with
  | `Assoc [
      ("data", `String data);
      ("type", `String "policy")] ->
    let lex = Lexing.from_string data in
    let pol = NetKAT_Parser.program NetKAT_Lexer.token lex in
    return pol

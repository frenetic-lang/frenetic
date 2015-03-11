(* NOTE(arjun): Core.Std.Int32.of_int_exn will throw an exception if it receives a positive
   integer value >= 0x1fffffff. However, OpenFlow ports are unsigned integers,
   so these are legitimate port numbers, IIRC. *)
let int_to_uint32 = Int32.of_int

(* NOTE(arjun): Do not open SDN_Types in this module. If you need to serialize
   one of those types, it should probably go in NetKAT_SDN_Json instead. *)
open Core.Std
open NetKAT_Types
open Yojson.Basic
open Optimize

let macaddr_to_string (mac : Int64.t) : string =
  let buf = Bytes.create 6 in
  let rec loop n =
    let byte = Int64.bit_and (Int64.shift_right mac (8 * n)) 0xffL in
    Bytes.set buf n (Char.of_int_exn (Int64.to_int_exn byte));
    if n = 5 then () else loop (n + 1) in
  loop 0;
  Macaddr.to_string (Macaddr.of_bytes_exn buf)

let macaddr_from_string (str : string) : Int64.t =
  let buf = Macaddr.to_bytes (Macaddr.of_string_exn str) in
  let byte n = Int64.of_int (Char.to_int (Bytes.get buf n)) in
  let rec loop n acc =
    let shift = 8 * n in
    let acc' = Int64.(acc + (shift_left (byte n) shift)) in
    if n = 5 then acc'
    else loop (n + 1) acc' in
  loop 0 0L

let to_json_value (h : header_val) : json = match h with
  | Switch n -> `Int (Int64.to_int_exn n)
  (* JavaScript can't represent large 64-bit numbers *)
  | EthSrc n
  | EthDst n -> `String (macaddr_to_string n)
  | Location (Physical n) -> `Assoc [("type", `String "physical");
                                     ("port", `Int (Int32.to_int_exn n))]
  | Location (Pipe s) -> `Assoc [("type", `String "pipe");
                                 ("name", `String s)]
  | Location (Query s) -> `Assoc [("type", `String "query");
                                  ("name", `String s)]
  | Vlan n
  | VlanPcp n
  | EthType n
  | IPProto n
  | TCPSrcPort n
  | TCPDstPort n -> `Int n
  | IP4Src (addr, mask)
  | IP4Dst (addr, mask) ->
    `Assoc [("addr", `String (Ipaddr.V4.to_string (Ipaddr.V4.of_int32 addr)));
            ("mask", `Int (Int32.to_int_exn mask))]

let to_json_header (h : header_val) : json =
  let str = match h with
    | Switch _ -> "switch"
    | Location _ -> "location"
    | EthSrc _ -> "ethsrc"
    | EthDst _ -> "ethdst"
    | Vlan _ -> "vlan"
    | VlanPcp _ -> "vlanpcp"
    | EthType _ -> "ethtype"
    | IPProto _ -> "ipproto"
    | IP4Src _ -> "ip4src"
    | IP4Dst _ -> "ip4dst"
    | TCPSrcPort _ -> "tcpsrcport"
    | TCPDstPort _ -> "tcpdstport" in
  `String str


let rec to_json_pred (pred : pred) : json = match pred with
  | True -> `Assoc [("type", `String "true")]
  | False -> `Assoc [("type", `String "false")]
  | Test h -> `Assoc [("type", `String "test");
                      ("header", to_json_header h);
                      ("value", to_json_value h)]
  | And (a, b) -> `Assoc [("type", `String "and");
                          ("preds", `List [to_json_pred a; to_json_pred b])]
  | Or (a, b) -> `Assoc [("type", `String "or");
                         ("preds", `List [to_json_pred a; to_json_pred b])]
  | Neg a -> `Assoc [("type", `String "neg");
                     ("pred", to_json_pred a)]

let rec policy_to_json (pol : policy) : json = match pol with
  | Filter a -> `Assoc [("type", `String "filter");
                        ("pred", to_json_pred a)]
  | Mod h -> `Assoc [("type", `String "mod");
                     ("header", to_json_header h);
                     ("value", to_json_value h)]
  | Union (p, q) ->
    `Assoc [("type", `String "union");
            ("pols", `List (flatten_union pol |> List.map ~f:policy_to_json))]
  | Seq (p, q) ->
    `Assoc [("type", `String "seq");
           ("pols", `List [policy_to_json p; policy_to_json q])]
  | Star p -> `Assoc [("type", `String "star");
                      ("pol", policy_to_json p)]
  | Link (sw1, pt1, sw2, pt2) ->
    `Assoc [("type", `String "link");
            ("sw1", `Int (Int64.to_int_exn sw1));
            ("pt1", `Int (Int32.to_int_exn pt1));
            ("sw2", `Int (Int64.to_int_exn sw2));
            ("pt2", `Int (Int32.to_int_exn pt2))]

let parse_ipaddr (json : json) : Int32.t =
  let open Yojson.Basic.Util in
  Ipaddr.V4.to_int32 (Ipaddr.V4.of_string_exn (to_string json))

let from_json_header_val (json : json) : header_val =
  let open Yojson.Basic.Util in
  let value = json |> member "value" in
  match json |> member "header" |> to_string with
  | "switch" -> Switch (value |> to_int |> Int64.of_int)
  | "location" ->
    let value = match value |> member "type" |> to_string with
      | "physical" -> Physical (value |> member "port" |>
                                to_int |> int_to_uint32)
      | "pipe" -> Pipe (value |> member "name" |> to_string)
      | "query" -> Query (value |> member "name" |> to_string)
      | str -> raise (Invalid_argument ("invalid location type " ^ str))
    in Location value
  | "ethsrc" -> EthSrc (value |> to_string |> macaddr_from_string)
  | "ethdst" -> EthDst (value |> to_string |> macaddr_from_string)
  | "vlan" -> Vlan (value |> to_int)
  | "vlanpcp" -> VlanPcp (value |> to_int)
  | "ethtype" -> EthType (value |> to_int)
  | "ipproto" -> IPProto (value |> to_int)
  | "ip4src" -> IP4Src (value |> member "addr" |> parse_ipaddr,
                        value |> member "mask" |> to_int |> Int32.of_int_exn)
  | "ip4dst" -> IP4Dst (value |> member "addr" |> parse_ipaddr,
                        value |> member "mask" |> to_int |> Int32.of_int_exn)
  | "tcpsrcport" -> TCPSrcPort (value |> to_int)
  | "tcpdstport" -> TCPDstPort (value |> to_int)
  | str -> raise (Invalid_argument ("invalid header " ^ str))

let rec from_json_pred (json : json) : pred =
  let open Yojson.Basic.Util in
  match json |> member "type" |> to_string with
  | "true" -> True
  | "false" -> False
  | "test" -> Test (from_json_header_val json)
  | "and" -> mk_big_and (json |> member "preds" |> to_list
                         |> List.map ~f:from_json_pred)
  | "or" -> mk_big_or (json |> member "preds" |> to_list
                       |> List.map ~f:from_json_pred)
  | "neg" -> Neg (json |> member "pred" |> from_json_pred)
  | str -> raise (Invalid_argument ("invalid predicate type " ^ str))

let rec policy_from_json (json : json) : policy =
  let open Yojson.Basic.Util in
   match json |> member "type" |> to_string with
   | "filter" -> Filter (json |> member "pred" |> from_json_pred)
   | "mod" -> Mod (from_json_header_val json)
   | "union" -> mk_big_union (json |> member "pols" |> to_list
                              |> List.map ~f:policy_from_json)
   | "seq" -> mk_big_seq (json |> member "pols" |> to_list
                          |> List.map ~f:policy_from_json)
   | "star" -> Star (policy_from_json (json |> member "pol"))
   | "link" -> Link (json |> member "sw1" |> to_int |> Int64.of_int,
                     json |> member "pt1" |> to_int |> int_to_uint32,
                     json |> member "sw2" |> to_int |> Int64.of_int,
                     json |> member "pt2" |> to_int |> int_to_uint32)
   | str -> raise (Invalid_argument ("invalid policy type " ^ str))


(* by default, Yojson produces non-standard JSON *)
let policy_to_json_string (pol : policy) : string =
  Yojson.Basic.to_string ~std:true (policy_to_json pol)

let policy_from_json_string (str : string) : policy =
  policy_from_json (from_string str)

let policy_from_json_channel (chan : In_channel.t) : policy =
  policy_from_json (from_channel chan)

let event_to_json (event : event) : json =
  let open Yojson.Basic.Util in
  match event with
  | PacketIn (pipe, sw_id, pt_id, payload, len) ->
    let buffer = SDN_Types.payload_bytes payload |>
      Cstruct.to_string |>
      B64.encode in
    `Assoc [
        ("type", `String "packet_in");
        ("pipe", `String pipe);
        ("switch_id", `Int (Int64.to_int_exn sw_id));
        ("port_id", `Int (Int32.to_int_exn pt_id));
        ("payload", `Assoc [
            ("buffer", `String buffer);
            ("id", match payload with
              | SDN_Types.Buffered (id, _) -> `Int (Int32.to_int_exn id)
              | _  -> `Null)
        ]);
        ("length", `Int len)
    ]
  | Query (name, pkt_count, byte_count) ->
    `Assoc [
      ("type", `String "query");
      ("packet_count", `Int (Int64.to_int_exn pkt_count));
      ("byte_count", `Int (Int64.to_int_exn byte_count))
    ]
  | SwitchUp (sw_id, ports) ->
     let json_ports = List.map ports (fun port -> `Int (Int32.to_int_exn port)) in
    `Assoc [
      ("type", `String "switch_up");
      ("switch_id", `Int (Int64.to_int_exn sw_id));
      ("ports", `List json_ports)
    ]
  | SwitchDown sw_id ->
    `Assoc [
      ("type", `String "switch_down");
      ("switch_id", `Int (Int64.to_int_exn sw_id))
    ]
  | PortUp (sw_id, pt_id) ->
    `Assoc [
      ("type", `String "port_up");
      ("switch_id", `Int (Int64.to_int_exn sw_id));
      ("port_id", `Int (Int32.to_int_exn pt_id))
    ]
  | PortDown (sw_id, pt_id) ->
    `Assoc [
      ("type", `String "port_down");
      ("switch_id", `Int (Int64.to_int_exn sw_id));
      ("port_id",   `Int (Int32.to_int_exn pt_id))
    ]
  | LinkUp ((sw_id1, pt_id1), (sw_id2, pt_id2)) ->
    `Assoc [
      ("type", `String "link_up");
      ("src", `Assoc [("switch_id", `Int (Int64.to_int_exn sw_id1));
                      ("port_id", `Int (Int32.to_int_exn pt_id1))]);
      ("dst", `Assoc [("switch_id", `Int (Int64.to_int_exn sw_id2));
                      ("port_id", `Int (Int32.to_int_exn pt_id2))])
    ]
  | LinkDown ((sw_id1, pt_id1), (sw_id2, pt_id2)) ->
    `Assoc [
        ("type", `String "link_down");
        ("src", `Assoc [("switch_id", `Int (Int64.to_int_exn sw_id1));
                        ("port_id", `Int (Int32.to_int_exn pt_id1))]);
        ("dst", `Assoc [("switch_id", `Int (Int64.to_int_exn sw_id2));
                        ("port_id", `Int (Int32.to_int_exn pt_id2))])
      ]
  | HostUp ((sw_id, pt_id), (dlAddr, nwAddr)) ->
    `Assoc [
      ("type", `String "host_up");
      ("switch_id", `Int (Int64.to_int_exn sw_id));
      ("port_id",   `Int (Int32.to_int_exn pt_id));
      ("dl_addr",   `String (Packet.string_of_dlAddr dlAddr));
      ("nw_addr",   `String (Packet.string_of_nwAddr nwAddr))
    ]
  | HostDown ((sw_id, pt_id), (dlAddr, nwAddr)) ->
    `Assoc [
      ("type", `String "host_down");
      ("switch_id", `Int (Int64.to_int_exn sw_id));
      ("port_id", `Int (Int32.to_int_exn pt_id));
      ("dl_addr", `String (Packet.string_of_dlAddr dlAddr));
      ("nw_addr", `String (Packet.string_of_nwAddr nwAddr))
    ]

let event_to_json_string (event : event) : string =
  Yojson.Basic.to_string ~std:true (event_to_json event)

let stats_to_json ((pkts, bytes) : Int64.t * Int64.t) : json =
  `Assoc [("packets", `Int (Int64.to_int_exn pkts));
          ("bytes", `Int (Int64.to_int_exn bytes))]

let stats_to_json_string (stats : Int64.t * Int64.t) : string =
  Yojson.Basic.to_string ~std:true (stats_to_json stats)

let port_stats_to_json (portStats : OpenFlow0x01_Stats.portStats) : json =
  `Assoc [("port_no", `Int portStats.port_no);
	  ("rx_packets", `Int (Int64.to_int_exn portStats.rx_packets));
	  ("tx_packets", `Int (Int64.to_int_exn portStats.tx_packets));
	  ("rx_bytes", `Int (Int64.to_int_exn portStats.rx_bytes));
	  ("tx_bytes", `Int (Int64.to_int_exn portStats.tx_bytes));
	  ("rx_dropped", `Int (Int64.to_int_exn portStats.rx_dropped));
	  ("tx_dropped", `Int (Int64.to_int_exn portStats.tx_dropped));
	  ("rx_errors", `Int (Int64.to_int_exn portStats.rx_errors));
	  ("tx_errors", `Int (Int64.to_int_exn portStats.tx_errors));
	  ("rx_fram_err", `Int (Int64.to_int_exn portStats.rx_frame_err));
	  ("rx_over_err", `Int (Int64.to_int_exn portStats.rx_over_err));
	  ("rx_crc_err", `Int (Int64.to_int_exn portStats.rx_crc_err));
	  ("collisions", `Int (Int64.to_int_exn portStats.collisions))]

let port_stats_to_json_string (portStats : OpenFlow0x01_Stats.portStats) : string =
  Yojson.Basic.to_string ~std:true (port_stats_to_json portStats)
	  

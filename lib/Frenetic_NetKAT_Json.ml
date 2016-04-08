(* NOTE(arjun): Core.Std.Int32.of_int_exn will throw an exception if it receives a positive
   integer value >= 0x1fffffff. However, OpenFlow ports are unsigned integers,
   so these are legitimate port numbers, IIRC. *)
let int_to_uint32 = Int32.of_int

(* NOTE(arjun): Do not open Frenetic_OpenFlow in this module. If you need to serialize
   one of those types, it should probably go in Frentic_NetKAT_SDN_Json instead. *)
open Core.Std
open Yojson.Basic
open Frenetic_NetKAT
open Frenetic_NetKAT_Optimize

(** IP & MAC Addresses **)
let string_of_mac = Frenetic_Packet.string_of_mac
let mac_of_string = Frenetic_Packet.mac_of_string

let string_of_ip = Frenetic_Packet.string_of_ip
let ip_of_string = Frenetic_Packet.ip_of_string

let to_json_ip (addr, mask : Frenetic_Packet.nwAddr * int32) : json =
  let addr = ("addr", `String (string_of_ip addr)) in
  let mask = Int32.to_int_exn mask |> function
    | 32 -> []
    | m -> [("mask", `Int m)] in
  `Assoc (addr :: mask)

let from_json_ip (json : json) : Frenetic_Packet.nwAddr * int32 =
  let open Yojson.Basic.Util in
  let addr = json |> member "addr" |> to_string |> ip_of_string in
  let mask = json |> member "mask" |> function
    | `Null -> 32 |> int_to_uint32
    | x -> x |> to_int |> int_to_uint32 in
  (addr, mask)

(** To JSON **)
let to_json_value (h : header_val) : json = match h with
  | Switch n | VSwitch n | VPort n | VFabric n -> `String (string_of_int (Int64.to_int_exn n))
  (* JavaScript can't represent large 64-bit numbers *)
  | EthSrc n
  | EthDst n -> `String (string_of_mac n)
  | Location (Physical n) -> `Assoc [("type", `String "physical");
                                     ("port", `Int (Int32.to_int_exn n))]
  (* TODO(grouptable) *)
  | Location (FastFail n_lst) -> failwith "Not Yet Implemented"
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
  | IP4Dst (addr, mask) -> to_json_ip (addr, mask)

let to_json_header (h : header_val) : json =
  let str = match h with
    | Switch _ -> "switch"
    | Location _ -> "location"
    | EthSrc _ -> "ethsrc"
    | EthDst _ -> "ethdst"
    | Vlan _ -> "vlan"
    | VlanPcp _ -> "vlanpcp"
    | VSwitch _ -> "vswitch"
    | VPort _ -> "vport"
    | EthType _ -> "ethtype"
    | IPProto _ -> "ipproto"
    | IP4Src _ -> "ip4src"
    | IP4Dst _ -> "ip4dst"
    | TCPSrcPort _ -> "tcpsrcport"
    | TCPDstPort _ -> "tcpdstport"
    | VFabric _ -> "vfabric" in
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
  | VLink (sw1, pt1, sw2, pt2) ->
    `Assoc [("type", `String "vlink");
            ("sw1", `Int (Int64.to_int_exn sw1));
            ("pt1", `Int (Int64.to_int_exn pt1));
            ("sw2", `Int (Int64.to_int_exn sw2));
            ("pt2", `Int (Int64.to_int_exn pt2))]


(** From JSON **)
let from_json_header_val (json : json) : header_val =
  let open Yojson.Basic.Util in
  let value = json |> member "value" in
  match json |> member "header" |> to_string with
  | "switch" -> Switch (value |> to_string |> Int64.of_string)
  | "vswitch" -> VSwitch (value |> to_string |> Int64.of_string)
  | "vport" -> VPort (value |> to_string |> Int64.of_string)
  | "location" ->
    let value = match value |> member "type" |> to_string with
      | "physical" -> Physical (value |> member "port" |>
                                to_int |> int_to_uint32)
      | "pipe" -> Pipe (value |> member "name" |> to_string)
      | "query" -> Query (value |> member "name" |> to_string)
      | str -> raise (Invalid_argument ("invalid location type " ^ str))
    in Location value
  | "port" -> Location(Physical(value |> to_string |> Int32.of_string))
  | "ethsrc" -> EthSrc (value |> to_string |> mac_of_string)
  | "ethdst" -> EthDst (value |> to_string |> mac_of_string)
  | "vlan" -> Vlan (value |> to_int)
  | "vlanpcp" -> VlanPcp (value |> to_int)
  | "ethtype" -> EthType (value |> to_int)
  | "ipproto" -> IPProto (value |> to_int)
  | "ip4src" -> let (x,y) = from_json_ip value in IP4Src (x,y)
  | "ip4dst" -> let (x,y) = from_json_ip value in IP4Dst (x,y)
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

let rec policy_of_json (json : json) : policy =
  let open Yojson.Basic.Util in
   match json |> member "type" |> to_string with
   | "filter" -> Filter (json |> member "pred" |> from_json_pred)
   | "mod" -> Mod (from_json_header_val json)
   | "union" -> mk_big_union (json |> member "pols" |> to_list
                              |> List.map ~f:policy_of_json)
   | "seq" -> mk_big_seq (json |> member "pols" |> to_list
                          |> List.map ~f:policy_of_json)
   | "star" -> Star (policy_of_json (json |> member "pol"))
   | "link" -> Link (json |> member "sw1" |> to_int |> Int64.of_int,
                     json |> member "pt1" |> to_int |> int_to_uint32,
                     json |> member "sw2" |> to_int |> Int64.of_int,
                     json |> member "pt2" |> to_int |> int_to_uint32)
   | str -> raise (Invalid_argument ("invalid policy type " ^ str))


(* by default, Yojson produces non-standard JSON *)
let policy_to_json_string (pol : policy) : string =
  Yojson.Basic.to_string ~std:true (policy_to_json pol)

let policy_of_json_string (str : string) : policy =
  policy_of_json (from_string str)

let policy_of_json_channel (chan : In_channel.t) : policy =
  policy_of_json (from_channel chan)

let event_to_json (event : event) : json =
  let open Yojson.Basic.Util in
  match event with
  | PacketIn (pipe, sw_id, pt_id, payload, len) ->
    let buffer = Frenetic_OpenFlow.payload_bytes payload |>
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
              | Frenetic_OpenFlow.Buffered (id, _) -> `Int (Int32.to_int_exn id)
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
      ("dl_addr",   `String (Frenetic_Packet.string_of_dlAddr dlAddr));
      ("nw_addr",   `String (Frenetic_Packet.string_of_nwAddr nwAddr))
    ]
  | HostDown ((sw_id, pt_id), (dlAddr, nwAddr)) ->
    `Assoc [
      ("type", `String "host_down");
      ("switch_id", `Int (Int64.to_int_exn sw_id));
      ("port_id", `Int (Int32.to_int_exn pt_id));
      ("dl_addr", `String (Frenetic_Packet.string_of_dlAddr dlAddr));
      ("nw_addr", `String (Frenetic_Packet.string_of_nwAddr nwAddr))
    ]

let event_to_json_string (event : event) : string =
  Yojson.Basic.to_string ~std:true (event_to_json event)

let stats_to_json ((pkts, bytes) : Int64.t * Int64.t) : json =
  `Assoc [("packets", `Int (Int64.to_int_exn pkts));
          ("bytes", `Int (Int64.to_int_exn bytes))]

let stats_to_json_string (stats : Int64.t * Int64.t) : string =
  Yojson.Basic.to_string ~std:true (stats_to_json stats)

let port_stat_to_json (portStat: Frenetic_OpenFlow0x01.portStats) : json =
  `Assoc [("port_no", `Int portStat.port_no);
    ("rx_packets", `Int (Int64.to_int_exn portStat.rx_packets));
    ("tx_packets", `Int (Int64.to_int_exn portStat.tx_packets));
    ("rx_bytes", `Int (Int64.to_int_exn portStat.rx_bytes));
    ("tx_bytes", `Int (Int64.to_int_exn portStat.tx_bytes));
    ("rx_dropped", `Int (Int64.to_int_exn portStat.rx_dropped));
    ("tx_dropped", `Int (Int64.to_int_exn portStat.tx_dropped));
    ("rx_errors", `Int (Int64.to_int_exn portStat.rx_errors));
    ("tx_errors", `Int (Int64.to_int_exn portStat.tx_errors));
    ("rx_fram_err", `Int (Int64.to_int_exn portStat.rx_frame_err));
    ("rx_over_err", `Int (Int64.to_int_exn portStat.rx_over_err));
    ("rx_crc_err", `Int (Int64.to_int_exn portStat.rx_crc_err));
    ("collisions", `Int (Int64.to_int_exn portStat.collisions))]

let port_stats_to_json (portStats : Frenetic_OpenFlow0x01.portStats list) : json =
  `List (List.map portStats ~f:port_stat_to_json)

let port_stats_to_json_string (portStats : Frenetic_OpenFlow0x01.portStats list) : string =
  Yojson.Basic.to_string ~std:true (port_stats_to_json portStats)


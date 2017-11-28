(* NOTE(arjun): Core.Int32.of_int_exn will throw an exception if it receives a positive
   integer value >= 0x1fffffff. However, OpenFlow ports are unsigned integers,
   so these are legitimate port numbers, IIRC. *)
let int_to_uint32 = Int32.of_int

(* NOTE(arjun): Do not open OpenFlow in this module. If you need to serialize
   one of those types, it should probably go in Frentic_SDN_Json instead. *)
open Core
open Yojson.Basic
open Syntax
open Optimize


(** Optimize & MAC Addresses **)

let string_of_mac = Frenetic_kernel.Packet.string_of_mac
let mac_of_string = Frenetic_kernel.Packet.mac_of_string

let string_of_ip = Frenetic_kernel.Packet.string_of_ip
let ip_of_string = Frenetic_kernel.Packet.ip_of_string

let to_json_ip (addr, mask : Frenetic_kernel.Packet.nwAddr * int32) : json =
  let addr = ("addr", `String (string_of_ip addr)) in
  let mask = Int32.to_int_exn mask |> function
    | 32 -> []
    | m -> [("mask", `Int m)] in
  `Assoc (addr :: mask)

let from_json_ip (json : json) : Frenetic_kernel.Packet.nwAddr * int32 =
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
  | From loc | AbstractLoc loc ->
    `String (Core.Sexp.to_string (sexp_of_abstract_location loc))
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
  (* SJS: can we use ppx_deriving_yojson to do this stuff automatically? *)
  | Meta _ -> failwith "meta fields not supported yet"

let to_json_header (h : header_val) : json =
  let str = match h with
    | Switch _ -> "switch"
    | Location _ -> "location"
    | From _ -> "from"
    | AbstractLoc _ -> "abstractloc"
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
    | VFabric _ -> "vfabric"
    | Meta _ -> failwith "meta fields not supported yet" in
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
  | Let _ -> failwith "meta fields not supported yet"
  | Dup -> failwith "dup not supported"


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

let rec pol_of_json (json : json) : policy =
  let open Yojson.Basic.Util in
   match json |> member "type" |> to_string with
   | "filter" -> Filter (json |> member "pred" |> from_json_pred)
   | "mod" -> Mod (from_json_header_val json)
   | "union" -> mk_big_union (json |> member "pols" |> to_list
                              |> List.map ~f:pol_of_json)
   | "seq" -> mk_big_seq (json |> member "pols" |> to_list
                          |> List.map ~f:pol_of_json)
   | "star" -> Star (pol_of_json (json |> member "pol"))
   | "link" -> Link (json |> member "sw1" |> to_int |> Int64.of_int,
                     json |> member "pt1" |> to_int |> int_to_uint32,
                     json |> member "sw2" |> to_int |> Int64.of_int,
                     json |> member "pt2" |> to_int |> int_to_uint32)
   | str -> raise (Invalid_argument ("invalid policy type " ^ str))

(* by default, Yojson produces non-standard JSON *)
let policy_to_json_string (pol : policy) : string =
  Yojson.Basic.to_string ~std:true (policy_to_json pol)

let pol_of_json_string (str : string) : policy =
  pol_of_json (from_string str)

let pol_of_json_channel (chan : In_channel.t) : policy =
  pol_of_json (from_channel chan)

let stats_to_json ((pkts, bytes) : Int64.t * Int64.t) : json =
  `Assoc [("packets", `Int (Int64.to_int_exn pkts));
          ("bytes", `Int (Int64.to_int_exn bytes))]

let stats_to_json_string (stats : Int64.t * Int64.t) : string =
  Yojson.Basic.to_string ~std:true (stats_to_json stats)

open Frenetic_kernel.OpenFlow
let pseudoport_from_json (json : json) : pseudoport =
  let open Yojson.Basic.Util in
  match json |> member "type" |> to_string with
   | "physical" -> Physical (json |> member "port" |> to_int |> Int32.of_int_exn)
   | "inport" -> InPort
   | "table" -> Table
   | "normal" -> Normal
   | "flood" -> Flood
   | "all" -> All
   | "controller" -> Controller (json |> member "bytes" |> to_int)
   | "local" -> Local
   | str -> failwith ("invalid pseudoport type: " ^ str)

let pseudoport_to_json (p : pseudoport) = match p with
  | Physical n -> `Assoc [("type", `String "physical");
                          ("port", `Int (Int32.to_int_exn n))]
  | InPort -> `Assoc [("type", `String "inport")]
  | Table -> `Assoc [("type", `String "table")]
  | Normal -> `Assoc [("type", `String "normal")]
  | Flood -> `Assoc [("type", `String "flood")]
  | All -> `Assoc [("type", `String "all")]
  | Local -> `Assoc [("type", `String "local")]
  | Controller n -> `Assoc [("type", `String "controller");
                            ("bytes", `Int n)]

let payload_from_json (json : json) : payload =
  let open Yojson.Basic.Util in
  match json |> member "type" |> to_string with
  | "notbuffered" ->
     let base64 = json |> member "data" |> to_string in
     NotBuffered (Cstruct.of_string (B64.decode base64))
  | "buffered" ->
    let bufferId = Int32.of_int_exn (json |> member "bufferid" |> to_int) in
    (* TODO(arjun): Why does Buffered take a second argument. Won't it be ignored
       if a buffer ID is specified? *)
    Buffered (bufferId, Cstruct.of_string "")
  | _ -> failwith "invalid payload"

let int32_option_from_json (json : json) : Int32.t option =
  let open Yojson.Basic.Util in
  match to_int_option json with
    | None -> None
    | Some n -> Some (Int32.of_int_exn n)

let policies_of_json (json: json) : policy list =
  let open Yojson.Basic.Util in
  json |> to_list |> List.map ~f:pol_of_json

let ingress_port_of_json (json: json) : portId option =
  let open Yojson.Basic.Util in
  match json |> to_int_option with
  | None -> None
  | Some n -> Int32.of_int n

let pkt_out_from_json (json : json) : switchId * portId option * payload * policy list =
  let open Yojson.Basic.Util in
  let switch = json |> member "switch" |> to_int |> Int64.of_int in
  let payload = json |> member "payload" |> payload_from_json in
  let policies = json |> member "policies" |> policies_of_json in
  let in_port = json |> member "in_port" |> ingress_port_of_json in
  (switch, in_port, payload, policies)

let pattern_to_json (p:Pattern.t) : json =
  let open Pattern in
  let str_field f = function
    | None -> `Null
    | Some x -> `String (f x) in
  let int_field f = function
    | None -> `Null
    | Some x -> `Int (f x) in
  `Assoc [
     ("dlSrc", str_field Frenetic_kernel.Packet.string_of_mac p.dlSrc);
     ("dlDst", str_field Frenetic_kernel.Packet.string_of_mac p.dlDst);
     ("dlTyp", int_field ident p.dlTyp);
     ("dlVlan", int_field ident p.dlVlan);
     ("dlVlanPcp", int_field ident p.dlVlanPcp);
     ("nwSrc", str_field Pattern.Ip.string_of p.nwSrc);
     ("nwDst", str_field Pattern.Ip.string_of p.nwDst);
     ("nwProto", int_field ident p.nwProto);
     ("tpSrc", int_field ident p.tpSrc);
     ("tpDst", int_field ident p.tpDst);
     ("inPort", int_field Int32.to_int_exn p.inPort) ]

let modify_to_json (m : modify) : json = match m with
  | SetEthSrc m ->
     `List [`String "SetDlSrc"; `String (Frenetic_kernel.Packet.string_of_mac m)]
  | SetEthDst m ->
     `List [`String "SetDlDst"; `String (Frenetic_kernel.Packet.string_of_mac m)]
  | SetVlan o ->
     `List [`String "SetVlan"; `Int (match o with None -> 0xffff | Some n ->  n)]
  | SetVlanPcp n ->
     `List [`String "SetVlanPcp"; `Int n]
  | SetEthTyp n ->
     `List [`String "SetDlTyp"; `Int n]
  | SetIPProto n ->
     `List [`String "SetNwProto"; `Int n]
  | SetIP4Src n ->
     `List [`String "SetNwSrc"; `String (Frenetic_kernel.Packet.string_of_ip n)]
  | SetIP4Dst n ->
     `List [`String "SetNwDst"; `String (Frenetic_kernel.Packet.string_of_ip n)]
  | SetTCPSrcPort n ->
     `List [`String "SetTpSrc"; `Int n]
  | SetTCPDstPort n ->
     `List [`String "SetTpDst"; `Int n]

let action_to_json (a : action) : json = match a with
  | Output p -> `List [`String "Output"; pseudoport_to_json p]
  | Enqueue (p, q) ->
     `List [`String "Enqueue";
            `Int (Int32.to_int_exn p);
            `Int (Int32.to_int_exn q)]
  | Modify m ->
     `List [`String "Modify"; modify_to_json m]
     (* TODO(grouptable): who gets this json? *)
  | FastFail p_lst -> failwith "Not Yet Implemented"

let seq_to_json (s : seq) : json =`List (List.map ~f:action_to_json s)

let par_to_json (p : par) : json = `List (List.map ~f:seq_to_json p)

let action_to_json (g : group) : json = match g with
  | [p] -> par_to_json p
  | _ -> failwith "NYI: serializing groups with multiple buckets"

let timeout_to_json (t : timeout) : json = match t with
  | Permanent -> `String "Permanent"
  | ExpiresAfter n -> `List [`String "ExpiresAfter"; `Int n]

let flow_to_json (n : int) (f : flow) : json =
  `Assoc [
     ("priority", `Int n);
     ("pattern", pattern_to_json f.pattern);
     ("action", action_to_json f.action);
     ("cookie", `String (Int64.to_string f.cookie));
     ("idle_timeout", timeout_to_json f.idle_timeout);
     ("hard_timeout", timeout_to_json f.hard_timeout)
   ]

let flowTable_to_json (tbl : flowTable) : json =
  let priorities = List.range ~stride:(-1) 65535 (65535 - List.length tbl) in
  `List (List.map2_exn ~f:flow_to_json priorities tbl)

let port_stat_to_json (portStat: Frenetic_kernel.OpenFlow.portStats) : json =
  `Assoc [("port_no", `Int (Int64.to_int_exn portStat.port_no));
    ("rx_packets", `Int (Int64.to_int_exn portStat.port_rx_packets));
    ("tx_packets", `Int (Int64.to_int_exn portStat.port_tx_packets));
    ("rx_bytes", `Int (Int64.to_int_exn portStat.port_rx_bytes));
    ("tx_bytes", `Int (Int64.to_int_exn portStat.port_tx_bytes));
    ("rx_dropped", `Int (Int64.to_int_exn portStat.port_rx_dropped));
    ("tx_dropped", `Int (Int64.to_int_exn portStat.port_tx_dropped));
    ("rx_errors", `Int (Int64.to_int_exn portStat.port_rx_errors));
    ("tx_errors", `Int (Int64.to_int_exn portStat.port_tx_errors));
    ("rx_fram_err", `Int (Int64.to_int_exn portStat.port_rx_frame_err));
    ("rx_over_err", `Int (Int64.to_int_exn portStat.port_rx_over_err));
    ("rx_crc_err", `Int (Int64.to_int_exn portStat.port_rx_crc_err));
    ("collisions", `Int (Int64.to_int_exn portStat.port_collisions))]

let port_stat_to_json_string (portStat: Frenetic_kernel.OpenFlow.portStats) : string =
  Yojson.Basic.to_string ~std:true (port_stat_to_json portStat)

let event_to_json (event : event) : json =
  let open Yojson.Basic.Util in
  match event with
  | PacketIn (pipe, sw_id, pt_id, payload, len, reason) ->
    let buffer = Frenetic_kernel.OpenFlow.payload_bytes payload |>
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
              | Frenetic_kernel.OpenFlow.Buffered (id, _) -> `Int (Int32.to_int_exn id)
              | _  -> `Null)
        ]);
        ("length", `Int len)
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
  | PortStats (sw_id, portStats) ->
    `List [ port_stat_to_json portStats ]
  | FlowStats (sw_id, flowStats) ->
    `Assoc [
      ("packets", `Int (Int64.to_int_exn flowStats.flow_packet_count));
      ("bytes", `Int (Int64.to_int_exn flowStats.flow_byte_count))
    ]

let event_to_json_string (event : event) : string =
  Yojson.Basic.to_string ~std:true (event_to_json event)


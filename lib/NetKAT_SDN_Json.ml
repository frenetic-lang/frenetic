open Core.Std
open Yojson.Basic
open SDN_Types

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

let action_from_json (json : json) : action =
  let open Yojson.Basic.Util in
  match json |> member "type" |> to_string with
    | "output" -> Output (json |> member "pseudoport" |> pseudoport_from_json)
    | "modify" -> failwith "NYI: parsing modify actions from JSON"
    | "enqueue" -> failwith "NYI: parsing enqueue actions from JSON"
    | str -> failwith ("invalid action type: " ^ str)

let pkt_out_from_json (json : json) : switchId * pktOut =
  let open Yojson.Basic.Util in
  let actions = json |> member "actions" |> to_list |>
    List.map ~f:action_from_json in
  let in_port = json |> member "in_port" |> int32_option_from_json  in
  let switch = json |> member "switch" |> to_int |> Int64.of_int in
  let packet = json |> member "payload" |> payload_from_json in
  (switch, (packet, in_port, actions))

let pattern_to_json (p:Pattern.t) : json =
  let open Pattern in
  let str_field f = function
    | None -> `Null
    | Some x -> `String (f x) in
  let int_field f = function
    | None -> `Null
    | Some x -> `Int (f x) in
  `Assoc [
     ("dlSrc", str_field Packet.string_of_mac p.dlSrc);
     ("dlDst", str_field Packet.string_of_mac p.dlDst);
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
     `List [`String "SetDlSrc"; `String (Packet.string_of_mac m)]
  | SetEthDst m ->
     `List [`String "SetDlDst"; `String (Packet.string_of_mac m)]
  | SetVlan o ->
     `List [`String "SetVlan"; `Int (match o with None -> 0xffff | Some n ->  n)]
  | SetVlanPcp n ->
     `List [`String "SetVlanPcp"; `Int n]
  | SetEthTyp n ->
     `List [`String "SetDlTyp"; `Int n]
  | SetIPProto n ->
     `List [`String "SetNwProto"; `Int n]
  | SetIP4Src n ->
     `List [`String "SetNwSrc"; `String (Packet.string_of_ip n)]
  | SetIP4Dst n ->
     `List [`String "SetNwDst"; `String (Packet.string_of_ip n)]
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
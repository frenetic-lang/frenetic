open Graph

(* Utility functions *)
let parse_rate (r:string) : Int64.t =
  let a = Str.search_forward (Str.regexp "\\([0-9]+\\)") r 0 in
  let amt = Str.matched_group 0 r in
  let _ = Str.search_forward (Str.regexp "\\([A-Za-z]+\\)") r a in
  let rate = Str.matched_group 0 r in
  let n = Int64.of_string amt in

  let m = match rate with
    | "bps" -> 1L
    | "Bps" -> 8L
    | "kbps" -> 1024L
    | "kBps" -> 8192L
    | "Mbps" -> 1048576L
    | "MBps" -> 8388608L
    | "Gbps" -> 1073741824L
    | "GBps" -> 8589934592L
    | _ -> failwith "Invalid rate specifier" in
  Int64.mul n m

let maybe o = match o with
  | Some(s) -> s
  | None -> failwith "Requires value"


(* Convert the generic id type to more specialized types *)

let string_of_id id = match id with
  | Dot_ast.Ident(s) -> s
  | Dot_ast.Number(s) -> "n" ^ s
  | Dot_ast.String(s) -> s
  | Dot_ast.Html(s) -> s

let int32_of_id vo = match maybe vo with
  | Dot_ast.Number(n) -> Int32.of_string n
  | _ -> failwith "Need a number to get int32\n"

let int64_of_id vo = match maybe vo with
  | Dot_ast.Number(n) -> Int64.of_string n
  | _ -> failwith "Need a number to get id\n"

let capacity_of_id vo = match maybe vo with
  | Dot_ast.String(s) -> parse_rate s
  | _ -> failwith "Need a string to get capacity\n"



module Node = struct
  type device = Switch | Host | Middlebox

  type t = { dev_type : device ;
             dev_id : int64 ;
             ip : int32 ;
             mac : int64 ;
             name : string }

  let default = { dev_type = Host ;
                  name = "" ;
                  ip = 0l ;
                  mac = 0L ;
                  dev_id = 0L }

  let compare = Pervasives.compare

  let to_string n = n.name

  let to_dot n =
    let devstr = match n.dev_type with
      | Switch -> "switch"
      | Host -> "host"
      | Middlebox -> "middlebox" in
    Printf.sprintf "%s [type=%s, ip=\"%s\", mac=\"%s\", id=%Ld]"
      n.name
      devstr
      (Packet.string_of_ip n.ip)
      (Packet.string_of_mac n.mac)
      (n.dev_id)


  (* Update the record for a node *)
  let update_dot_attr n (k,vo) =
    let dev_type_of vo = match string_of_id (maybe vo) with
      | "host" -> Host
      | "switch" -> Switch
      | "middlebox" -> Middlebox
      | s -> failwith (Printf.sprintf "Unknown node type: %s\n" s)
    in
    let ip_of vo = match maybe vo with
      | Dot_ast.String(s) -> Packet.ip_of_string s
      | _ -> failwith "IPs must be represented as a string (in quotes)\n" in
    let mac_of vo = match maybe vo with
      | Dot_ast.String(s) -> Packet.mac_of_string s
      | _ -> failwith "MAC must be represented as a string (in quotes)\n" in
    match k with
      | Dot_ast.Ident("type") -> {n with dev_type = dev_type_of vo}
      | Dot_ast.Ident("id") -> {n with dev_id = int64_of_id vo}
      | Dot_ast.Ident("ip") -> {n with ip = ip_of vo}
      | Dot_ast.Ident("mac") -> {n with mac = mac_of vo}
      | _ -> failwith "Unknown node attribute\n"

  let parse_dot (i:Dot_ast.node_id) (ats:Dot_ast.attr list) : t =
    let (id, popt) = i in
    let name = string_of_id id in
    let at = List.hd ats in
    List.fold_left update_dot_attr
      {default with name = name} at

  let int64_of_value v = match v with
    | Gml.Int(i) -> Int64.of_int i
    | _ -> failwith "Id requires int value\n"

  let string_of_value v = match v with
    | Gml.String(s) -> s
    | _ -> failwith "Label requires int value\n"

  let update_gml_attr n (key, value) =
    match key with
      | "id" -> {n with dev_id = int64_of_value value}
      | "label" -> {n with name = string_of_value value}
      | "mac" -> {n with mac = Packet.mac_of_string (string_of_value value)}
      | "ip" -> {n with ip = Packet.ip_of_string (string_of_value value)}
      | _ -> n

  let parse_gml (vs:Gml.value_list) : t =
    List.fold_left update_gml_attr default vs
end

module Link = struct
  type t = { cost : int64 ;
             capacity : int64 ; }

  let default = { cost = 1L;
                  capacity = Int64.max_int }

  let compare = Pervasives.compare

  let to_string (l:t) : string =
    Printf.sprintf "{ cost = %s; capacity = %s; }"
      (Int64.to_string l.cost)
      (Int64.to_string l.capacity)

  let to_dot = to_string

  let update_dot_attr edge (key,valopt) =
    match key with
      | Dot_ast.Ident("cost") -> {edge with cost = int64_of_id valopt }
      | Dot_ast.Ident("capacity") -> {edge with capacity = capacity_of_id valopt }
      | _ -> failwith "Unknown edge attribute\n"

  let update_gml_attr edge (key, value) = match key with
    | _ -> edge

  let parse_dot (ats:Dot_ast.attr list) : t =
    let at = List.hd ats in
    let link = List.fold_left update_dot_attr default at in
    link

  let parse_gml (vs:Gml.value_list) : t =
    let link = List.fold_left update_gml_attr default vs in
    link
end

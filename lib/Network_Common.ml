open Graph
open Network
open Sexplib.Conv

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

  type device = Switch | Host | Middlebox with sexp, compare

  type t = { dev_type : device ;
             dev_id : int64 ;
             ip : int32 ;
             mac : int64 ;
             name : string } with sexp, compare

  type partial_t = { partial_dev_type : device option ;
                     partial_dev_id : int64 option ;
                     partial_ip : int32 option ;
                     partial_mac : int64 option ;
                     partial_name : string option }

  let default = { dev_type = Host ;
                  dev_id = 0L ;
                  name   = "" ;
                  ip     = 0l ;
                  mac    = 0L }

  let partial_default = { partial_dev_type = None ;
                            partial_dev_id = None ;
                            partial_ip     = None ;
                            partial_mac    = None ;
                            partial_name   = None }


  let create (n:string) (i:int64) (d:device) (ip:int32) (mac:int64) : t =
    { dev_type = d ;
      name = n ;
      ip = ip ;
      mac = mac ;
      dev_id = i }

  let name (n:t) : string = n.name
  let id (n:t) : int64 = n.dev_id
  let device (n:t) : device = n.dev_type
  let mac (n:t) : int64 = n.mac
  let ip (n:t) : int32 = n.ip

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

  let to_mininet n = match n.dev_type with
    | Host ->
      (* Mininet doesn't like underscores in host names *)
      let mnname = Str.global_replace (Str.regexp "_") "" n.name in
      Printf.sprintf "%s = net.addHost(\'%s\', mac=\'%s\', ip=\'%s\')\n"
        n.name mnname
        (Packet.string_of_mac n.mac) (Packet.string_of_ip n.ip)
    | _ ->
      Printf.sprintf
        "%s = net.addSwitch(\'s%Ld\')\n" n.name n.dev_id


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
      | Dot_ast.Ident("type") -> {n with partial_dev_type = Some (dev_type_of vo)}
      | Dot_ast.Ident("id") -> {n with partial_dev_id = Some (int64_of_id vo)}
      | Dot_ast.Ident("ip") -> {n with partial_ip = Some (ip_of vo)}
      | Dot_ast.Ident("mac") -> {n with partial_mac = Some (mac_of vo)}
      | _ -> failwith "Unknown node attribute\n"

  (* Take the partial node record and remove the option types, or
     raise an error if it is not fully filled *)
  let unbox (p:partial_t) : t =
    let unbox_host (p:partial_t) =
      let i = match p.partial_ip with
        | Some i -> i
        | None -> failwith "Host must have an IP address" in
      let m = match p.partial_mac with
        | Some m -> m
        | None -> failwith "Host must have a MAC address" in
      let n = match p.partial_name with
        | Some n -> n
        | None -> failwith "Host must have a name" in
      let id = match p.partial_dev_id with
        | Some i -> i
        | None -> m in
      { dev_type = Host ; dev_id = id ; ip = i ; mac = m ; name = n} in

    let unbox_switch (p:partial_t) =
      let id = match p.partial_dev_id with
        | Some i -> i
        | None -> failwith "Switches must have a unique id" in
      let n = match p.partial_name with
        | Some n -> n
        | None -> failwith "Switch must have a name" in
      let m = match p.partial_mac with
        | Some m -> m
        | None -> 0L in
      let i = match p.partial_ip with
        | Some i -> i
        | None -> 0l in
      { dev_type = Switch ; dev_id = id ; ip = i ; mac = m ; name = n} in

    match p.partial_dev_type with
    | Some Host -> unbox_host p
    | Some Switch -> unbox_switch p
    | Some Middlebox -> unbox_switch p
    | _ -> failwith "Must provide valid devide type for all nodes"


  let parse_dot (i:Dot_ast.node_id) (ats:Dot_ast.attr list) : t =
    let (id, popt) = i in
    let name = string_of_id id in
    let at = List.hd ats in
    let partial = List.fold_left update_dot_attr
      {partial_default with partial_name = Some name} at in
    unbox partial

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
             capacity : int64 ;
             mutable weight : float } with sexp, compare

  let default = { cost = 1L;
                  capacity = Int64.max_int;
                  weight = 1. }

  let create (cost:int64) (cap:int64) : t =
    { default with cost = cost; capacity = cap }

  let cost (l:t) = l.cost
  let capacity (l:t) = l.capacity

  let compare = Pervasives.compare

  let weight (l:t) = l.weight
  let set_weight (l:t) (w:float) = l.weight <- w

  let to_string (l:t) : string =
    Printf.sprintf " cost = %s; capacity = %s; "
      (Int64.to_string l.cost)
      (Int64.to_string l.capacity)

  let to_dot = to_string

  let update_dot_attr edge (key,valopt) =
    match key with
      | Dot_ast.Ident("cost") -> {edge with cost = int64_of_id valopt }
      | Dot_ast.Ident("capacity") -> {edge with capacity = capacity_of_id valopt }
      | Dot_ast.Ident(s) -> edge
      | _ -> failwith ("Unknown edge attribute\n")

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

module Weight = struct
  type label = Link.t with sexp, compare
  type t = float with sexp, compare
  let weight l =
    let open Link in
    l.weight
  let compare = compare
  let add = (+.)
  let zero = 0.
end

module Net = Network.Make(Node)(Link)
module NetPath = Net.Path(Weight)

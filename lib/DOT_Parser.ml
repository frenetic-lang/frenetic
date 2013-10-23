open Util
open DOT_Types
open Topology
open Graph


module TopoDot = struct
  let parse_rate (r:string) : Int64.t =
    let a = Str.search_forward (Str.regexp "\\([0-9]+\\)") r 0 in
    let amt = Str.matched_group 0 r in
    let _ = Str.search_forward (Str.regexp "\\([A-Za-z]+\\)") r a in
    let rate = Str.matched_group 0 r in
    let n = Int64.of_string amt in

    let m = match rate with
      | "Bps" -> 1L
      | "kbps" -> 128L
      | "kBps" -> 1024L
      | "Mbps" -> 131072L
      | "MBps" -> 1048576L
      | "Gbps" -> 134217728L
      | "GBps" -> 1073741824L
      | "Tbps" -> 137438953472L
      | "TBps" -> 1099511627776L
      | _ -> failwith "Invalid rate specifier" in
    Int64.mul n m

  let print_id id = match id with
    | Dot_ast.Ident(s) -> Printf.printf "Ident: %s\n" s
    | Dot_ast.Number(s) -> Printf.printf "Number: %s\n" s
    | Dot_ast.String(s) -> Printf.printf "String: %s\n" s
    | Dot_ast.Html(s) -> Printf.printf "Html: %s\n" s

  let print_attr (a,o) =
    let _ = print_id a in
    let _ = match o with
      | None -> Printf.printf "None"
      | Some(i) -> print_id i in
    Printf.printf "\n"

  let name_of_id id = match id with
    | Dot_ast.Ident(s) -> s
    | Dot_ast.Number(s) ->
      failwith (Printf.sprintf "Cannot use number %s as name of node\n" s)
    | Dot_ast.String(s) -> s
    | Dot_ast.Html(s) ->
      failwith (Printf.sprintf "Cannot use HTML %s as name of node\n" s)

  let id_of vo = match vo with
    | Some(i) -> begin match i with
        | Dot_ast.Number(n) -> Int64.of_string n
        | _ -> failwith "Need a number to get id\n" end
    | None ->  failwith "Need a number to get id\n"

  let int32_of_id vo = match vo with
    | Some(i) -> begin match i with
        | Dot_ast.Number(n) -> Int32.of_string n
        | _ -> failwith "Need a number to get int32\n" end
    | None ->  failwith "Need a number to get int32\n"

  let capacity_of_id vo = match vo with
    | Some(i) -> begin match i with
        | Dot_ast.String(s) -> parse_rate s
        | _ -> failwith "Need a string to get capacity\n" end
    | None ->  failwith "Need a value to get capacity\n"

  let update_nattr n (k,vo) =
    let ntype_of vo = match vo with
      | Some(s) -> name_of_id s
      | None -> failwith "Node attributes must have a value.\n"
    in
    let ip_of vo = match vo with
      | Some(i) -> begin match i with
          | Dot_ast.String(s) -> s
          | _ -> failwith "Need a number to get id\n" end
      | None ->  failwith "Need a number to get id\n" in
    match k with
      | Dot_ast.Ident("type") -> {n with DOT_Types.ntype = ntype_of vo}
      | Dot_ast.Ident("id") -> {n with DOT_Types.id = id_of vo}
      | Dot_ast.Ident("ip") -> {n with DOT_Types.ip = ip_of vo}
      | _ -> failwith "Unknown node attribute\n"

  let update_eattr edge (key,valopt) =
    match key with
      | Dot_ast.Ident("sport") -> {edge with Link.srcport = int32_of_id valopt}
      | Dot_ast.Ident("dport") -> {edge with Link.dstport = int32_of_id valopt}
      | Dot_ast.Ident("cost") -> {edge with Link.cost = id_of valopt}
      | Dot_ast.Ident("capacity") -> {edge with Link.capacity = capacity_of_id valopt}
      | _ -> failwith "Unknown node attribute\n"

  let node (i:Dot_ast.node_id) (ats:Dot_ast.attr list) : Topology.V.label =
    let (id, popt) = i in
    let name = name_of_id id in
    let at = List.hd ats in
    let nat = List.fold_left update_nattr DOT_Types.defnattr at in
    if nat.ntype = "host" then
      Node.Host(name)
    else if nat.ntype = "switch" then
      Node.Switch(name, nat.id)
    else
      Node.Mbox(name,[])

  let edge (ats:Dot_ast.attr list) : Topology.E.label =
    let at = List.hd ats in
    let link = List.fold_left update_eattr Link.default at in
    link
    (* Printf.printf "\n\nEATTR:\n"; *)
    (* let _ = List.iter (fun at -> Printf.printf "outer\n";List.iter print_attr at) ats in *)
    (* Link.default *)


end


module P = Graph.Dot.Parse(Graph.Builder.P(Topology))(TopoDot)
let dot_parse = P.parse
exception ParseError of info * string
      (* Private utility functions *)
let get_int s =
  let _ = try Str.search_forward (Str.regexp "[0123456789]+") s 0
    with Not_found -> failwith
      (Printf.sprintf "Cannot guess switch identifier for %s\n" s)
  in
  let num = Str.matched_string s in
  try
    Int64.of_string num
  with Failure "int_of_string" ->
    failwith (Printf.sprintf "Invalid ID for %s\n" s)

let mk_host n m =
  try StringMap.find n m
  with Not_found ->
    if n.[0] = 'h' then Node.Host n
    else if n.[0] = 's' then Node.Switch(n, get_int n)
    else if n.[0] = 'm' then Node.Mbox (n, [])
    else failwith (Printf.sprintf "Invalid host type for %s\n" n)

let mk_edges n1 n2 attrs m =
  let h1 = mk_host n1 m in
  let h2 = mk_host n2 m in
  let l = {Link.srcport = attrs.sport; Link.dstport = attrs.dport;
           Link.cost = attrs.cost; Link.capacity = attrs.capacity} in
  let l' = {Link.srcport = attrs.dport; Link.dstport = attrs.sport;
            Link.cost = attrs.cost; Link.capacity = attrs.capacity} in
  let fwd = Link.mk_edge h1 h2 l in
  let bwd = Link.mk_edge h2 h1 l' in
  (fwd,bwd)

open Graph
open Util

module NH = Node.NodeHash
module TopoDot = struct
  let nodeinfo = NH.create 100
  let index = ref 0

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

  (* Update the record for a node *)
  let update_nattr n (k,vo) =
    let open Node in
    let ntype_of vo = match string_of_id (maybe vo) with
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
      | Dot_ast.Ident("type") -> {n with node_type = ntype_of vo}
      | Dot_ast.Ident("id") -> {n with dev_id = int64_of_id vo}
      | Dot_ast.Ident("ip") -> {n with ip = ip_of vo}
      | Dot_ast.Ident("mac") -> {n with mac = mac_of vo}
      | _ -> failwith "Unknown node attribute\n"

  (* Update the record for an edge *)
  let update_eattr edge (key,valopt) =
    match key with
      | Dot_ast.Ident("sport") -> {edge with Link.srcport = int64_of_id valopt}
      | Dot_ast.Ident("dport") -> {edge with Link.dstport = int64_of_id valopt}
      | Dot_ast.Ident("cost") -> {edge with Link.cost = int64_of_id valopt }
      | Dot_ast.Ident("capacity") -> {edge with Link.capacity = capacity_of_id valopt }
      | _ -> failwith "Unknown edge attribute\n"

  (* Generate a node from the id and attributes *)
  let node (i:Dot_ast.node_id) (ats:Dot_ast.attr list) : Network.G.V.label =
    let (id, popt) = i in
    let name = string_of_id id in
    let at = List.hd ats in
    let nattr = List.fold_left update_nattr
        {Node.default with Node.name = name} at in
    let node = Node.create !index in
    index := !index + 1;
    NH.replace nodeinfo node nattr;
    node


  (* Generate a link from the attributes *)
  let edge (ats:Dot_ast.attr list) : Network.G.E.label =
    let at = List.hd ats in
    let link = List.fold_left update_eattr Link.default at in
    link

end

module TopoGML = struct
  let nodeinfo = NH.create 100
    let index = ref 0
    let int32_of_value v = match v with
      | Gml.Int(i) -> Int32.of_int i
      | _ -> failwith "source and target require int values\n"

    let int64_of_value v = match v with
      | Gml.Int(i) -> Int64.of_int i
      | _ -> failwith "Id requires int value\n"

    let string_of_value v = match v with
      | Gml.String(s) -> s
      | _ -> failwith "Label requires int value\n"

    let update_nattr n (key, value) =
      let open Node in match key with
      | "id" -> {n with dev_id = int64_of_value value}
      | "label" -> {n with name = string_of_value value}
      | "mac" -> {n with mac = Packet.mac_of_string (string_of_value value)}
      | "ip" -> {n with ip = Packet.ip_of_string (string_of_value value)}
      | _ -> n

    (* The Src and Dst nodes are handled by Ocamlgraph. So we use the source and
       target fields to assign ports such that each switch is connected to
       every other switch on a port id equal to its node id. Helps keep things
       simple, since most Topology Zoo graphs don't seem to have port
       information *)
    let update_eattr edge (key, value) = match key with
      | "source" -> {edge with Link.dstport = int64_of_value value}
      | "target" -> {edge with Link.srcport = int64_of_value value}
      | _ -> edge

    let node (vs:Gml.value_list) : Network.G.V.label =
      let nat = List.fold_left update_nattr
        Node.default vs in
      let node = Node.create !index in
      index := !index + 1;
      NH.replace nodeinfo node nat;
      node


    let edge (vs:Gml.value_list) : Network.G.E.label =
      let link = List.fold_left update_eattr Link.default vs in
      link
end

module B = Graph.Builder.P(Network.G)
module GML = Graph.Gml.Parse(B)(TopoGML)
module DOT = Graph.Dot.Parse(B)(TopoDot)

let from_dotfile s = (DOT.parse s, TopoDot.nodeinfo)
let from_gmlfile s = (GML.parse s, TopoGML.nodeinfo)

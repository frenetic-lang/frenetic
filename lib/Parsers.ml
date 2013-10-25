open Util
open Topology
open Graph

type nattr = {
  ntype: string
  ; name : string
  ; id : int64
  ; ip : string
}

let defnattr = {ntype = "host"; name = ""; id = 0L; ip = "0.0.0.0"}

module TopoDot = struct

  (* Utility functions *)
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
    let ntype_of vo = string_of_id (maybe vo) in
    let ip_of vo = match maybe vo with
          | Dot_ast.String(s) -> s
          | _ -> failwith "IPs must be represented as a string (in quotes)\n" in
    match k with
      | Dot_ast.Ident("type") -> {n with ntype = ntype_of vo}
      | Dot_ast.Ident("id") -> {n with id = int64_of_id vo}
      | Dot_ast.Ident("ip") -> {n with ip = ip_of vo}
      | _ -> failwith "Unknown node attribute\n"

  (* Update the record for an edge *)
  let update_eattr edge (key,valopt) =
    match key with
      | Dot_ast.Ident("sport") -> {edge with Link.srcport = int32_of_id valopt}
      | Dot_ast.Ident("dport") -> {edge with Link.dstport = int32_of_id valopt}
      | Dot_ast.Ident("cost") -> {edge with Link.cost = int64_of_id valopt}
      | Dot_ast.Ident("capacity") -> {edge with Link.capacity = capacity_of_id valopt}
      | _ -> failwith "Unknown node attribute\n"

  (* Generate a node from the id and attributes *)
  let node (i:Dot_ast.node_id) (ats:Dot_ast.attr list) : Topology.V.label =
    let (id, popt) = i in
    let name = string_of_id id in
    let at = List.hd ats in
    let nat = List.fold_left update_nattr defnattr at in
    if nat.ntype = "host" then
      Node.Host(name)
    else if nat.ntype = "switch" then
      Node.Switch(name, nat.id)
    else
      Node.Mbox(name,[])

  (* Generate a link from the attributes *)
  let edge (ats:Dot_ast.attr list) : Topology.E.label =
    let at = List.hd ats in
    let link = List.fold_left update_eattr Link.default at in
    link

end

module TopoGML = struct

    let int32_of_value v = match v with
      | Gml.Int(i) -> Int32.of_int i
      | _ -> failwith "source and target require int values\n"

    let int64_of_value v = match v with
      | Gml.Int(i) -> Int64.of_int i
      | _ -> failwith "Id requires int value\n"

    let string_of_value v = match v with
      | Gml.String(s) -> s
      | _ -> failwith "Label requires int value\n"

    let update_nattr n (key, value) = match key with
      | "id" -> {n with id = int64_of_value value}
      | "label" -> {n with name = string_of_value value}
      | _ -> n

    (* The Src and Dst nodes are handled by Ocamlgraph. So we use the source and
       target fields to assign ports such that each switch is connected to
       every other switch on a port id equal to its node id. Helps keep things
       simple, since most Topology Zoo graphs don't seem to have port
       information *)
    let update_eattr edge (key, value) = match key with
      | "source" -> {edge with Link.dstport = int32_of_value value}
      | "target" -> {edge with Link.srcport = int32_of_value value}
      | _ -> edge

    let node (vs:Gml.value_list) : Topology.V.label =
      let nat = List.fold_left update_nattr
        {defnattr with name = "switch"} vs in
      if nat.ntype = "host" then
        Node.Host(nat.name)
      else if nat.ntype = "switch" then
        Node.Switch(nat.name, nat.id)
      else
        Node.Mbox(nat.name,[])


    let edge (vs:Gml.value_list) : Topology.E.label =
      let link = List.fold_left update_eattr Link.default vs in
      link
end

module B = Graph.Builder.P(Topology)
module GML = Graph.Gml.Parse(B)(TopoGML)
module DOT = Graph.Dot.Parse(B)(TopoDot)

let from_dotfile = DOT.parse
let from_gmlfile = GML.parse

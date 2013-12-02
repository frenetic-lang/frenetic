open Topology_Util
open Graph
module Core = Topology_Core
open Core 

type nattr = {
  ntype: string
  ; name : string
  ; id : int64
  ; ip : string
  ; mac : string
}

let defnattr = {ntype = "host"; name = ""; id = 0L; ip = "0.0.0.0"; mac = "00:00:00:00:00:00"}

module TopoDot = struct
    let name2attrs = ref (Hashtbl.create 100)
    let id2attrs = ref (Hashtbl.create 100)
    let index = ref 0

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
    let mac_of vo = match maybe vo with
          | Dot_ast.String(s) -> s
          | _ -> failwith "MAC must be represented as a string (in quotes)\n" in
    match k with
      | Dot_ast.Ident("type") -> {n with ntype = ntype_of vo}
      | Dot_ast.Ident("id") -> {n with id = int64_of_id vo}
      | Dot_ast.Ident("ip") -> {n with ip = ip_of vo}
      | Dot_ast.Ident("mac") -> {n with mac = mac_of vo}
      | _ -> failwith "Unknown node attribute\n"

  (* Update the record for an edge *)
  let update_eattr edge (key,valopt) =
    match key with
      | Dot_ast.Ident("sport") -> {edge with Core.Link.srcport = VInt.Int32 (int32_of_id valopt)}
      | Dot_ast.Ident("dport") -> {edge with Core.Link.dstport = VInt.Int32 (int32_of_id valopt)}
      | Dot_ast.Ident("cost") -> {edge with Core.Link.cost = VInt.Int64 (int64_of_id valopt) }
      | Dot_ast.Ident("capacity") -> {edge with Core.Link.capacity = VInt.Int64 (capacity_of_id valopt)}
      | _ -> failwith "Unknown edge attribute\n"

  (* Generate a node from the id and attributes *)
  let node (i:Dot_ast.node_id) (ats:Dot_ast.attr list) : Core.Topology.V.label =
    let (id, popt) = i in
    let name = string_of_id id in
    let at = List.hd ats in
    let nat = List.fold_left update_nattr
        {defnattr with ntype = "switch"; name = name} at in
    Hashtbl.replace !name2attrs name nat;
    if nat.ntype = "host" then
      Core.Node.Host(name, VInt.Int64 (Packet.mac_of_string nat.mac),
                     VInt.Int32 (Packet.ip_of_string nat.ip))
    else if nat.ntype = "switch" then begin
      Hashtbl.replace !id2attrs (VInt.Int64 nat.id) nat;
      Core.Node.Switch(VInt.Int64 nat.id) end
    else
      Core.Node.Mbox(name,[])


  (* Generate a link from the attributes *)
  let edge (ats:Dot_ast.attr list) : Core.Topology.E.label =
    let at = List.hd ats in
    let link = List.fold_left update_eattr Core.Link.default at in
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
      | "mac" -> {n with mac = string_of_value value}
      | "ip" -> {n with ip = string_of_value value}
      | _ -> n

    (* The Src and Dst nodes are handled by Ocamlgraph. So we use the source and
       target fields to assign ports such that each switch is connected to
       every other switch on a port id equal to its node id. Helps keep things
       simple, since most Topology Zoo graphs don't seem to have port
       information *)
    let update_eattr edge (key, value) = match key with
      | "source" -> {edge with Core.Link.dstport = VInt.Int32 (int32_of_value value)}
      | "target" -> {edge with Core.Link.srcport = VInt.Int32 (int32_of_value value)}
      | _ -> edge

    let node (vs:Gml.value_list) : Core.Topology.V.label =
      let nat = List.fold_left update_nattr
        {defnattr with ntype = "switch"} vs in
      if nat.ntype = "host" then
        Core.Node.Host(nat.name, VInt.Int64 (Packet.mac_of_string nat.mac),
                       VInt.Int32 (Packet.ip_of_string nat.ip))
      else if nat.ntype = "switch" then
        Core.Node.Switch(VInt.Int64 nat.id)
      else
        Core.Node.Mbox(nat.name,[])


    let edge (vs:Gml.value_list) : Core.Topology.E.label =
      let link = List.fold_left update_eattr Core.Link.default vs in
      link
end

module B = Graph.Builder.P(Core.Topology)
module GML = Graph.Gml.Parse(B)(TopoGML)
module DOT = Graph.Dot.Parse(B)(TopoDot)

let from_dotfile_tbl s =
  (DOT.parse s, !TopoDot.name2attrs, !TopoDot.id2attrs)

let from_dotfile = DOT.parse
let from_gmlfile = GML.parse

(* TODO(basus): add a mininet parser *)

(* let from_mininet_raw (lst : (node * portId * node) list) = *)
(*   let open Node in  *)
(*   let g = Topology.empty in *)
(*   let len = 500 in *)
(*   let weight x y = match (x, y) with *)
(*     | (Host _, Host _) -> 1 *)
(*     | (Host _, Switch _) -> len *)
(*     | (Switch _, Host _) -> len *)
(*     | (Switch _, Switch _) -> 1 in *)
(*   List.iter  *)
(*     (fun (src,portId,dst) ->   *)
(*       Topology.add_edge g src portId (weight src dst) dst)  *)
(*     lst; *)
(*   g *)

(* let from_mininet filename =  *)
(*   from_mininet_raw (Mininet.parse_from_chan (open_in filename) filename) *)

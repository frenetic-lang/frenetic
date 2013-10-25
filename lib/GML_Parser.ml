open Util
open DOT_Types
open Topology
open Graph

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
      | "id" -> {n with DOT_Types.id = int64_of_value value}
      | "label" -> {n with DOT_Types.name = string_of_value value}
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
        {DOT_Types.defnattr with name = "switch"} vs in
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

module G = Graph.Gml.Parse(Graph.Builder.P(Topology))(TopoGML)
let gml_parse = G.parse

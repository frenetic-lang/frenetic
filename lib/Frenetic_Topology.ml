(* Topology utility functions. This module should eventually be replaced with a
 * Frenetic-specific topology module that includes the ocaml-topology module.
 *)

open Core.Std

module Net = Frenetic_NetKAT_Net.Net
module SDN = Frenetic_OpenFlow

let switch_ids (t : Net.Topology.t) : SDN.switchId list =
  let open Net.Topology in
  fold_vertexes (fun v acc ->
    match vertex_to_label t v with
    | Frenetic_NetKAT_Net.Switch id -> id::acc
    | _ -> acc)
  t []

(* Topology detection doesn't really detect hosts. So, I treat any
   port not connected to a known switch as an edge port *)
let internal_ports (t : Net.Topology.t) (sw_id : SDN.switchId) =
  let open Net.Topology in
  let switch = vertex_of_label t (Switch sw_id) in
  PortSet.fold  (vertex_to_ports t switch) ~init:PortSet.empty ~f:(fun acc p ->
    match next_hop t switch p with
    | Some e ->
       let node, _ = edge_dst e in
       begin match vertex_to_label t node with
       | Switch _ -> PortSet.add acc p
       | _ -> acc
       end
    | _ -> acc)

let in_edge (t : Net.Topology.t) (sw_id : SDN.switchId) (pt_id : SDN.portId) =
  let open Net.Topology in
  let switch = vertex_of_label t (Switch sw_id) in
  match next_hop t switch pt_id with
  | None    -> true
  | Some(_) -> false

let edge (t: Net.Topology.t) =
  let open Net.Topology in
  fold_vertexes (fun v acc ->
    match vertex_to_label t v with
    | Frenetic_NetKAT_Net.Switch sw_id ->
      PortSet.fold (vertex_to_ports t v) ~init:acc ~f:(fun acc pt_id ->
        match next_hop t v pt_id with
        | None   -> (sw_id, pt_id)::acc
        | Some _ -> acc)
    | _ -> acc)
  t []


module CoroNode = struct

  open Frenetic_Packet
  open Frenetic_NetKAT

  type t =
    | Switch of string * switchId
    | Repeater of switchId
    | Host of string * dlAddr * nwAddr
  [@@deriving sexp, compare]

  let to_string t = match t with
    | Switch(name, id) -> name
    | Repeater id      -> sprintf "r%Lu" id
    | Host(name, dlAddr, nwAddr) -> name

  let parse_dot _ _ = failwith "Cannot parse a Coronet node from DOT format"
  let parse_gml _   = failwith "Cannot parse a Coronet node from GML format"

  let to_dot t = match t with
    | Switch(name, id) ->
      sprintf "switch %s [label=%Lu]" name id
    | Repeater id ->
      sprintf "repeater %Lu" id
    | Host(name, dlAddr, nwAddr) ->
      sprintf "%s [label=%s]" (to_string t) (string_of_nwAddr nwAddr)

  let to_mininet n = match n with
    | Switch(name, id) ->
      sprintf "%s = net.addSwitch(\'s%Ld\', dpid=\'%s\', cls=LINCSwitch)\n"
        name id (string_of_mac id)
    | Repeater(id) ->
      sprintf "r%Ld = net.addSwitch(\'r%Ld\', cls=LINCSwitch)\n" id id
    | Host(name, mac, ip) ->
      (* Mininet doesn't like underscores in host names *)
      let mnname = Str.global_replace (Str.regexp "_") "" name in
      sprintf "%s = net.addHost(\'%s\', mac=\'%s\', ip=\'%s\')\n"
        name mnname
        (string_of_mac mac) (string_of_ip ip)

end

module CoroLink = struct
  type t = float [@@deriving sexp, compare]

  let to_string = string_of_float

  let default = 0.0

  let parse_dot _ = failwith "Cannot parse a Coronet link from DOT format"
  let parse_gml _ = failwith "Cannot parse a Coronet link from GML format"

  let to_dot = to_string

end


module CoroNet = struct
  include Frenetic_Network.Make(CoroNode)(CoroLink)

  let starts_with s c = match String.index s c with
    | Some i -> i = 0
    | None -> false

  let from_csv_file filename =
    let net = Topology.empty () in
    let channel = In_channel.create filename in

    let next_id    = ref 0L in
    let id_table   = Hashtbl.Poly.create () in
    let port_table = Hashtbl.Poly.create () in

    let get_id name = match Hashtbl.Poly.find id_table name with
      | None ->
        Int64.incr next_id;
        Hashtbl.Poly.add_exn id_table name ( !next_id );
        !next_id
      | Some i -> i in

    let get_port name = match Hashtbl.Poly.find port_table name with
      | None ->
        Hashtbl.Poly.add_exn port_table name 0l;
        0l
      | Some p ->
        Hashtbl.Poly.set port_table name (Int32.succ p);
        Int32.succ p in

    In_channel.fold_lines channel ~init:net ~f:(fun net line ->
        if starts_with line '#' then net
        else
          match String.split line ~on:',' with
          | sname::dname::[dist] ->
            let sid = get_id sname in
            let sport = get_port sname in
            let net,src = Topology.add_vertex net ( Switch(sname,sid) ) in

            let did = get_id dname in
            let dport = get_port dname in
            let net,dst = Topology.add_vertex net ( Switch(dname,did) ) in

            let distance = float_of_string dist in
            (* Add edges in both directions because the topology structure is directional *)
            let net,_ = Topology.add_edge net src sport distance dst dport in
            let net,_ = Topology.add_edge net dst dport distance src sport in
            net
          | _ -> failwith "Expected each line in CSV to have structure `src,dst,distance`"
      )

end

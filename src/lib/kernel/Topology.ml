(* Topology utility functions. This module should eventually be replaced with a
 * Frenetic-specific topology module that includes the ocaml-topology module.
 *)

open Core
module SDN = OpenFlow
module Net = struct
  include Net.Net
  include Net
end

let switch_ids (t : Net.Topology.t) : SDN.switchId list =
  let open Net.Topology in
  fold_vertexes (fun v acc ->
    match vertex_to_label t v with
    | Net.Switch id -> id::acc
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
    | Net.Switch sw_id ->
      PortSet.fold (vertex_to_ports t v) ~init:acc ~f:(fun acc pt_id ->
        match next_hop t v pt_id with
        | None   -> (sw_id, pt_id)::acc
        | Some _ -> acc)
    | _ -> acc)
  t []

module Mininet = struct

  type location =
    | Switch of int * int
    | Host of int

  let geo_sum a r n = a * ((1 - (Int.pow r n)) / (1 - r))

  let (--) i j =
    let rec aux n acc = if n < i then acc else aux (n-1) (n :: acc)
    in aux j []

  let single host_count =
    let children = 1 -- host_count in
    let topo_down, _ = List.fold children ~init:([], 1)
        ~f:(fun (acc, port) to_host ->
            ((Switch(1, port), Host(to_host)) :: acc), port + 1) in
    let topo_up = List.map topo_down (fun (x, y) -> y, x) in
    topo_up @ topo_down

  let minimal = single 2

  let linear switch_count =
    let hosts = List.map (1 -- switch_count) (fun x -> (Switch(x, 1), Host(x))) in
    let switches =
      if switch_count > 1 then
        (Switch(2, 2), Switch(1, 2)) :: List.map (2 -- (switch_count - 1)) (fun x -> (Switch((x + 1), 2), Switch(x, 3)))
      else
        [] in
    let topo_down = hosts @ switches in
    let topo_up = List.map topo_down (fun (x, y) -> y, x) in
    topo_up @ topo_down

  let tree depth fanout =
    let rec add_hosts switch fanout position =
      let children = (((position - 1) * fanout) + 1) -- (position * fanout) in
      let switches, top_port = List.fold children ~init:([], 1)
          ~f:(fun (acc, port) to_host ->
              ((Switch(switch, port), Host(to_host)) :: acc), port + 1)
      in switches in
    let rec add_switches switch fanout =
      let children = (((switch - 1) * fanout) + 2) -- ((switch * fanout) + 1) in
      let switches, top_port = List.fold children ~init:([], 1)
          ~f:(fun (acc, port) to_sw ->
              ((Switch(switch, port), Switch(to_sw, (fanout + 1))) :: acc), port + 1)
      in switches in
    let switch_numbers    = 1 -- (geo_sum 1 fanout (depth - 1)) in
    let switch_with_hosts = List.zip_exn
        (((geo_sum 1 fanout (depth - 1)) + 1) -- (geo_sum 1 fanout depth))
        (1 -- (Int.pow fanout (depth - 1))) in
    let sw_links = List.fold switch_numbers ~init:[] ~f:(fun acc sw -> add_switches sw fanout @ acc) in
    let topo_down = List.fold switch_with_hosts ~init:sw_links ~f:(fun acc (sw, pos) -> add_hosts sw fanout pos @ acc) in
    let topo_up = List.map topo_down (fun (x, y) -> y, x) in
    topo_up @ topo_down

  let location_tuples_to_topology location_tuples =
    let location_to_id loc = match loc with
      | Switch (id, port) -> Network.Node.Switch, id
      | Host id -> Network.Node.Host, id in

    (* Get all unique locations *)
    let locations =
      List.fold location_tuples ~init:[] ~f:(fun acc (x, y) -> 
        location_to_id x :: location_to_id y :: acc
      )
      |> List.dedup_and_sort
    in

    (* Get a topology with all vertices *)
    let topo_with_vertexes, loc_tup_to_vertex_table = List.fold locations ~init:(Network.Net.Topology.empty (), [])
        ~f:(fun (old_topo, acc) loc ->
            let device, id = loc in
            let name, ip, mac = match device with
              | Network.Node.Switch -> "s" ^ (Int.to_string id), 0l, 0L
              | Network.Node.Host -> "h" ^ (Int.to_string id), Int32.(of_int_exn id + 167772160l), Int64.of_int id
              | Network.Node.Middlebox -> "m" ^ (Int.to_string id), 0l, 0L in
            let vertex_node = Network.Node.create name (Int64.of_int id) device ip mac in
            let topo, vertex = Network.Net.Topology.add_vertex old_topo vertex_node in
            (topo, (loc, vertex) :: acc)) in

    (* Function to find a vertex from location *)
    let find_vertex loc = List.Assoc.find_exn loc_tup_to_vertex_table ~equal:(=)
        (location_to_id loc) in

    (* Add all the edges to the topo *)
    List.fold location_tuples ~init:topo_with_vertexes ~f:(fun old_topo (from_loc, to_loc) ->
        let loc_to_port loc = match loc with
          | Switch (id, port)-> Int32.of_int_exn port
          | Host id -> 1l in
        let topo, _ = Network.Net.Topology.add_edge old_topo
            (find_vertex from_loc) (loc_to_port from_loc)
            (Network.Link.default)
            (find_vertex to_loc) (loc_to_port to_loc) in
        topo)

  type topo_name =
    | Tree of int * int
    | Linear of int
    | Single of int
    | Minimal

  let topo_from_name name =
    (match name with
      | Tree (x, y) -> tree x y
      | Linear x -> linear x
      | Single x -> single x
      | Minimal -> minimal)
    |> location_tuples_to_topology

end

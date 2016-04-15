open Core.Std
open Frenetic_Network
open Frenetic_OpenFlow0x01

type fabric = (switchId, Frenetic_OpenFlow0x01.flowMod list) Hashtbl.t

let blank_pattern =
  { dlSrc = None
  ; dlDst = None
  ; dlTyp = None
  ; dlVlan = None
  ; dlVlanPcp = None
  ; nwSrc = None
  ; nwDst = None
  ; nwProto = None
  ; nwTos = None
  ; tpSrc = None
  ; tpDst = None
  ; inPort = None }

let add_flow =
  { command = AddFlow
  ; pattern = blank_pattern
  ; actions = []
  ; priority = 65535
  ; cookie = 0L
  ; idle_timeout = Permanent
  ; hard_timeout = Permanent
  ; notify_when_removed = true
  ; apply_to_packet = None
  ; out_port = None
  ; check_overlap = false
  }

let drop = { add_flow with priority = 1 }

let vlan_per_port (net:Net.Topology.t) : fabric =
  let mk_flow_mod (port:int) : flowMod =
    let pattern = { blank_pattern with dlVlan = Some (Some port) } in
    let actions = [ SetDlVlan None; Output (PhysicalPort port) ] in
    { add_flow with pattern = pattern; actions = actions;
      out_port = Some (PhysicalPort port) }
  in

  let open Net.Topology in
  let tags = Hashtbl.Poly.create ~size:(num_vertexes net) () in
  iter_edges (fun edge ->
      let src, port = edge_src edge in
      let label = vertex_to_label net src in
      let flow_mod = mk_flow_mod (Int32.to_int_exn port) in
      match Node.device label with
      | Node.Switch ->
        Hashtbl.Poly.change tags (Node.id label)
          ~f:(fun table -> match table with
          | Some flow_mods -> Some( flow_mod::flow_mods )
          | None -> Some [flow_mod; drop] )
      | _ -> ()) net;
  tags

let shortest_path (net:Net.Topology.t)
    (ingress:switchId list) (egress:switchId list) : fabric =
  let open Net.Topology in
  let vertexes = vertexes net in
  let vertex_from_id swid =
    let vopt = VertexSet.find vertexes (fun v ->
      (Node.id (vertex_to_label net v)) = swid) in
    match vopt with
    | Some v -> v
    | None -> failwith (Printf.sprintf "No vertex for switch id: %Ld" swid )
  in

  let mk_flow_mod (tag:int) (port:int) : flowMod =
    let pattern = { blank_pattern with dlVlan = Some (Some tag) } in
    let actions = [ Output (PhysicalPort port) ] in
    { add_flow with pattern = pattern; actions = actions;
      out_port = Some (PhysicalPort port) }
  in


  let table = Hashtbl.Poly.create ~size:(num_vertexes net) () in
  let tag = ref 10 in
  List.iter ingress ~f:(fun swin ->
    let src = vertex_from_id swin in
    List.iter egress ~f:(fun swout ->
      if swin = swout then ()
      else
        let dst = vertex_from_id swout in
        tag := !tag + 1;
        match Net.UnitPath.shortest_path net src dst with
        | None -> ()
        | Some p ->
          List.iter p ~f:(fun edge ->
            let src, port = edge_src edge in
            let label = vertex_to_label net src in
            let flow_mod = mk_flow_mod !tag (Int32.to_int_exn port) in
            match Node.device label with
            | Node.Switch ->
              Hashtbl.Poly.change table (Node.id label)
                ~f:(fun table -> match table with
                | Some flow_mods -> Some( flow_mod::flow_mods )
                | None -> Some [flow_mod; drop] )
            | _ -> ())));
  table

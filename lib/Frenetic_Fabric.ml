open Core.Std
open Frenetic_Network
open Frenetic_OpenFlow0x01

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

let vlan_per_port (net:Net.Topology.t) :
    (switchId, Frenetic_OpenFlow0x01.flowMod list) Hashtbl.t =
  let mk_flow_mod (port:int) : flowMod =
    let pattern = { blank_pattern with dlVlan = Some (Some port) } in
    let actions = [ SetDlVlan None; Output (PhysicalPort port) ] in
    { add_flow with pattern = pattern; actions = actions }
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
